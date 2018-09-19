/**
 * @file parser.c
 * this file contains the declaration of parsing functionailties, the
 * main method here is parse_sexpr()
 *
 * @see @file vector.h
 * @see @file token.h
 *
 * @todo this would take a set of tokens and then creates a parse-tree
 * the process of parsing has duplication in it!
 */

#include "../include/parser.h"

#include "../include/vector.h"
#include "../include/token.h"

#include "../include/pair.h"
#include "../include/sexpr.h"

/**
 * @brief this function is resposible of turning a set of tokens into a
 * s-expression.
 *
 * first it reads a set of tokens and then it parses each one as
 * described below:
 *
 *    + if it was a list, then parse_as_list()
 *    + if it was either a number/string/symbol then parse_as_x()
 *    + if it was was quote then parse_as_quote()
 *    + if the token was and error-token, then stop the process!
 */
sexpr_t *parse_sexpr(vector_t * tokens) {
    sexpr_t *expr = NULL, *value = NULL;
    sexpr_t *head = NULL, *tail = NULL;
    sexpr_t *nil = sexpr_nil();
    token_t *token = NULL;

    while ((token = vector_peek(tokens))) {
#if PARSER_DEBUG == DBG_ON
	puts("current token:");
	token_print(token);
	putchar('\n');
#endif

	switch (token->type) {
	case TOK_ERR:
	    token_free(token);
	    goto FAILED;

	case TOK_L_PAREN:
	    value = parse_as_list(tokens);
	    break;

	case TOK_QUOTE:
	    value = parse_as_quote(tokens);
	    break;

	case TOK_NUMBER:
	    value = parse_as_number(token->vbuffer);
	    break;
	case TOK_SYMBOL:
	    value = parse_as_symbol(token->vbuffer);
	    break;
	case TOK_STRING:
	    value = parse_as_string(token->vbuffer);
	    break;

	default:
	    break;
	}

	token_free(token);

	if (tokens->size == 0 && !head) {
	    head = value;
	    break;
	}

	expr = cons(value, nil);
	expr->c->ishead = !head;

	if (!head)
	    head = expr;
	else
	    set_cdr(tail, expr);

	tail = expr;
    }

    return head;

  FAILED:
    return NULL;
}

vector_t *parse_sexprs(vector_t * vtokens) {
    int i;
    vector_t *v = vector_new(NULL, sexpr_print, NULL);

    for (i = 0; i < vtokens->size; ++i) {
#if PARSER_DEBUG == DBG_ON
	puts(" ========== vector of tokens =========== ");
	vector_print(vector_get(vtokens, i));
	puts(" ========== ================ =========== ");
	sexpr_t *tmp =
#endif
	    vector_push(v, parse_sexpr(vector_get(vtokens, i)));

#if PARSER_DEBUG == DBG_ON
	printf("parsed sexpr: ");
	sexpr_print(tmp);
#endif
    }

    return v;
}

/* ===================================================================
** NOTE: there is no check parens here, the lexer did the job
** FIXME: improve the code
*/
sexpr_t *parse_as_list(vector_t * tokens) {
    sexpr_t *expr = NULL, *value = NULL;
    sexpr_t *head = NULL, *tail = NULL;
    sexpr_t *nil = sexpr_nil();

    token_t *token = NULL;
    bool isfirstloop = true;

#if PARSER_DEBUG == DBG_ON
    puts("list starting");
#endif

    while ((token = vector_peek(tokens))) {
#if PARSER_DEBUG == DBG_ON
	puts("current token:");
	token_print(token);
	putchar('\n');
#endif

	if (token->type == TOK_R_PAREN) {
	    token_free(token);
	    if (isfirstloop)	/* '() empty list is like a nil */
		return nil;
	    else		/* reached the end of the list */
		break;
	}

	switch (token->type) {
	case TOK_L_PAREN:
	    value = parse_as_list(tokens);
	    break;

	case TOK_QUOTE:
	    value = parse_as_quote(tokens);
	    break;

	case TOK_NUMBER:
	    value = parse_as_number(token->vbuffer);
	    break;
	case TOK_STRING:
	    value = parse_as_string(token->vbuffer);
	    break;
	case TOK_SYMBOL:
	    value = parse_as_symbol(token->vbuffer);
	    break;

	default:

#if PARSER_DEBUG == DBG_ON
	    puts("UNKNOWN SYMBOL");
#endif
	    return NULL;	/* this would cause problems */
	}

	token_free(token);

	expr = cons(value, nil);
	expr->c->ishead = !head;

	if (!head)
	    head = expr;
	else
	    set_cdr(tail, expr);

	tail = expr;

	isfirstloop = false;
    }

#if PARSER_DEBUG == DBG_ON
    sexpr_print(head);
    puts("list ending\n--------------\n");
#endif

    return head;
}

sexpr_t *parse_as_quote(vector_t * tokens) {
    token_t *token;
    sexpr_t *quote = NULL, *value = NULL;

    token = vector_peek(tokens);

    switch (token->type) {
    case TOK_L_PAREN:
	value = parse_as_list(tokens);
	break;

    case TOK_NUMBER:
	value = parse_as_number(token->vbuffer);
	break;
    case TOK_STRING:
	value = parse_as_string(token->vbuffer);
	break;
    case TOK_SYMBOL:
	value = parse_as_symbol(token->vbuffer);
	break;

    default:
	puts("UNKNOWN SYMBOL");
	return NULL;		/* this would cause problems */
    }

    token_free(token);

    if (isnil(value))
	return value;

    quote = sexpr_new(T_SYMBOL);
    quote->s = strdup("quote");

    value = cons(quote, cons(value, sexpr_nil()));

#if PARSER_DEBUG == DBG_ON
    sexpr_print(value);
#endif

    return value;
}

sexpr_t *parse_as_number(string_t value) {
    assert(value != NULL);
    sexpr_t *expr = sexpr_new(T_NUMBER);
    expr->n = strtod(value, NULL);
    return expr;
}

sexpr_t *parse_as_string(string_t value) {
    assert(value != NULL);
    sexpr_t *expr = sexpr_new(T_STRING);
    expr->s = strdup(value);
    return expr;
}

sexpr_t *parse_as_symbol(string_t value) {
    sexpr_t *expr = NULL;
    expr = parse_as_string(value);
    expr->type = !strcmp(value, "nil") ? T_NIL : T_SYMBOL;
    return expr;
}

#include "../include/lexer.h"

void parser_testing(void) {
    string_t exprs[] = {
	"(+ 11111 (* 22222 33333))",
	/* "(define bar '(* 22222 33333))", */
	/* "(define bar 'b)", */
	"(quote (a b c))" "(quote a)"
	    /* "    ; this is cool\n(bar baz)", */
	    /* "(define square (lambda (n) (* n n)))", */
	    /* "(\"this is a string\")	     " */
    };

    /* vector_t *tmp = read_tokens("(quote (a b c))"); */
    /* sexpr_t *q = parse_sexpr(tmp); */

    /* sexpr_describe(q); */
    /* puts("----------"); */
    /* sexpr_describe(car(q)); */
    /* puts("----------"); */
    /* sexpr_describe(car(cdr(q))); */
    /* puts("----------"); */
    /* vector_free(tmp); */

    int i, size = sizeof(exprs) / sizeof(exprs[0]);
    vector_t *v = NULL;
    sexpr_t *expr = NULL;

    for (i = 0; i < size; ++i) {
	printf("\n + parsing %s\n", exprs[i]);

	v = read_tokens(exprs[i]);

	puts("\n + list of tokens");
	vector_print(v);
	puts("-----------\n");

	expr = parse_sexpr(v);
	puts("\n + parsed expression");
	sexpr_print(expr);
	puts(" ================== ================= ================= ");

	vector_free(v);
    }
}
