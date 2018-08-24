/*
 * this would take a set of tokens and then creates a parse-tree
 * PS: or something like that
 *
 * the process of parsing has duplication in it!
 *
 * removed EOL for the moment, it's not important as it seems!
 *
 * it's a design failure i'm trying to create an interpreter, which
 * is a software that reads text and generate desired output based on
 * a well specified syntax which i'm trying to parse right now.
 * this is really fun yet very hard, or kinda
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
 *    + if the token was and error-token, then stop the process!
 *    + if it was a list, then parse_as_list
 *    + if it was
 */
sexpr_t *parse_sexpr(vector_t * tokens) {
    sexpr_t *expr = NULL, *value = NULL;
    sexpr_t *head = NULL, *tail = NULL;
    token_t *token = NULL;

    while ((token = vector_peek(tokens))) {
	token_print(token);

	switch (token->type) {
	case TOK_ERR:
	    vector_free(tokens);
	    goto FAILED;

	case TOK_L_PAREN:
	    expr = parse_as_list(tokens);
	    break;

	case TOK_NUMBER:
	    expr = parse_as_number(token->vbuffer);
	    break;
	case TOK_SYMBOL:
	    expr = parse_as_symbol(token->vbuffer);
	    break;
	case TOK_STRING:
	    expr = parse_as_string(token->vbuffer);
	    break;

	default:
	    break;
	}

	/* ====================== testing this ====================== */
	expr = cons(value, sexpr_new(T_NIL));

	if (!head) {
	    head = expr;
	} else {
	    set_cdr(tail, expr);
	}

	tail = expr;
	/* ========================================================== */
	sexpr_describe(head);
    }

    vector_free(tokens);

    return head;

  FAILED:

	gc_collect(true);
	return NULL;
}

sexpr_t *parse_token(token_t * token) {
    return NULL;
}

/* ===================================================================
** NOTE: there is no check parens here, the lexer did the job
** FIXME: improve the code
*/
sexpr_t *parse_as_list(vector_t * tokens) {
    sexpr_t *expr = NULL;
    sexpr_t *head = NULL, *tail = NULL, *value = NULL;

    token_t *token = NULL;
    bool_t isfirstloop = true;

    while ((token = vector_peek(tokens))) {
	token_print(token);
	if (token->type == TOK_R_PAREN) {
	    if (isfirstloop)
		return sexpr_new(T_NIL);	/* '() */
	    else
		break;
	}

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
	    value = sexpr_new(T_NIL);
	    break;
	}

	expr = cons(value, sexpr_new(T_NIL));

	if (!head) {
	    head = expr;
	} else {
	    set_cdr(tail, expr);
	}

	tail = expr;

	token_free(token);
	isfirstloop = false;

	sexpr_describe(head);
    }

    return head;
}

sexpr_t *parse_as_number(string_t value) {
    assert(value != NULL);

    sexpr_t *expr = sexpr_new(T_NUMBER);

    expr->v.n = strtod(value, NULL);

    return expr;
}

sexpr_t *parse_as_string(string_t value) {
    sexpr_t *expr = sexpr_new(T_STRING);

    expr->v.s = strdup(value);

    return expr;
}

sexpr_t *parse_as_boolean(string_t value) {
    sexpr_t *expr = sexpr_new(T_BOOLEAN);

    if (!strcmp(value, "nil") || !strcmp(value, "#f")) {
	expr->v.b = false;
    } else if (!strcmp(value, "#t") || !strcmp(value, "t")) {
	expr->v.b = true;
    }

    return NULL;
}

/*
expr: 0x557dfac88740, type:4 (CONS-PAIR)
    content:
    expr: 0x557dfac886e0, type:3 (ATOM)
	content: +

 */
sexpr_t *parse_as_symbol(string_t value) {
    sexpr_t *expr;

    if (!(expr = parse_as_boolean(value))) {
	expr = parse_as_string(value);
	expr->type = T_ATOM;
	assert(expr != NULL);
    }

    return expr;
}

#if PARSER_DEBUG == DBG_ON
#  include "../include/lexer.h"

void parser_testing(void) {
    string_t exprs[] = {
	"(+ 11111 (* 22222 33333))",
	"(\"this is a string\")	 ",
	"    ; this is cool\n(bar baz)"
    };

    int i, size = sizeof(exprs) / sizeof(exprs[0]);
    vector_t *v = NULL;
    sexpr_t *expr = NULL;

    for (i = 0; i < size; ++i) {
	printf("\n + parsing %s\n", exprs[i]);

	v = read_tokens(exprs[i]);

	puts("\n + list of tokens");
	vector_print(v);


	expr = parse_sexpr(v);
	puts("\n + parsed expression");
	sexpr_describe(expr);

	vector_free(v);
    }

    /* memory should be freed using GC
     * but for the moment it is not! */
}
#endif
