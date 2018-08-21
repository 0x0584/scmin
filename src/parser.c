/*
 * this would take a set of tokens and then creates a parse-tree
 * PS: or something like that
 */

#include "../include/parser.h"

#include "../include/vector.h"
#include "../include/token.h"

#include "../include/pair.h"
#include "../include/sexpr.h"

sexpr_t *parse_sexpr(vector_t * tokens) {
    int i;
    sexpr_t *expr = NULL;
    token_t *token = NULL;

    while (true) {
	token = vector_pop(tokens);

	switch (token->type) {
	case TOK_L_PAREN:
	    expr = parse_as_list(tokens);
	    break;

	case TOK_NUMBER:
	    expr = parse_as_number(token->vbuffer);
	    break;
	case TOK_ATOM:
	    expr = parse_as_atom(token->vbuffer);
	    break;
	case TOK_D_QUOTE:
	    expr = parse_as_string(token->vbuffer);
	    break;

	default:
	    break;
	}

	if (token->type == TOK_R_PAREN && token->depth == 0) {
	    break;		/* we have finished parsing */
	}
    }

    assert(expr != NULL);

    vector_free(tokens);	/* free the tokens in the parser does
				 * not seem like a good idea. or is it? */

    return expr;
}

sexpr_t *parse_as_list(vector_t * tokens) {
    string_t error[] = {
	"LIST HAS NO CLOSE PAREN"
    };
    int noerror = 0;
    bool_t isfinished = false, isfirstloop = true;
    sexpr_t *expr;
    sexpr_t *car = NULL, *cdr = NULL, *value;

    token_t *token = NULL;

    while (true) {
	token = vector_pop(tokens);

	if (token->type == TOK_R_PAREN) {
	    /* empty list check '() */
	    if (isfirstloop) return sexpr_new(T_NIL);
	    isfinished = true;
	    break;
	} else {
	    switch (token->type) {
	    case TOK_NUMBER:
		value = parse_as_number(token->vbuffer);
		break;
	    case TOK_D_QUOTE:
		value = parse_as_string(token->vbuffer);
		break;
	    case TOK_ATOM:
		value = parse_as_atom(token->vbuffer);
		break;
	    case TOK_L_PAREN:
		value = parse_as_list(tokens);
		break;
	    default:
		break;
	    }
	}

	expr = sexpr_new(T_PAIR);
	expr->v.c = cons(value, NULL);

	if (!cdr) {
	    car = expr;
	} else {
	    set_cdr(cdr, expr);
	}

	cdr = expr;

	token_free(token);
	isfirstloop = false;
    }

    if (!isfinished) {
	noerror = 0;
	goto FAILED;
    }

    return expr;

  FAILED:

    raise_error(stderr, error[noerror]);
    return NULL;
}

sexpr_t *parse_as_number(string_t value) {
    string_t error[] = {
	"NUMBER PARSING ERROR!"
    };
    int i = 0, noerror;

    sexpr_t *expr = sexpr_new(T_NUMBER);
    number_t val;
    unsigned int count_chars = 0;

    count_chars = sscanf(value, "%lf\n", &val);

    if (strlen(value) == count_chars) {
	expr->v.n = val;
    } else {
	noerror = 0;
	goto FAILED;
    }

    return expr;

  FAILED:

    raise_error(stderr, error[noerror]);
    return NULL;
}

sexpr_t *parse_as_string(string_t value) {
    string_t error[] = {
	"STRING PARSING ERROR!"
    };
    int noerror;

    sexpr_t *expr = sexpr_new(T_STRING);

    expr->v.s = value;

    if (expr->v.s == NULL) {
	noerror = 0;
	goto FAILED;
    }

    return expr;

  FAILED:
    raise_error(stderr, error[noerror]);
    return NULL;
}

sexpr_t *parse_as_boolean(string_t value) {
    string_t error[] = {
	"ERROR WHILE PARSING BOOLEAN"
    };
    int noerror;

    sexpr_t *expr = sexpr_new(T_BOOLEAN);

    if (!strcmp(value, "nil") || !strcmp(value, "#f")) {
	expr->v.b = false;
    } else if (!strcmp(value, "#t") || !strcmp(value, "t")) {
	expr->v.b = true;
    } else {
	noerror = 0;
	goto FAILED;
    }

    return expr;

  FAILED:
    raise_error(stderr, error[noerror]);
    return NULL;
}

sexpr_t *parse_as_atom(string_t value) {
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
	"(\"this is a string\")	 ",
	"(+ 4512 (* 45 2054))",
	"(car \'((foo bar) (fuzz buzz)))",
	"    ; this is cool\n(bar baz)"
    };
    puts(">>>");
    int i, size = sizeof(exprs) / sizeof(exprs[0]);
    vector_t *v = NULL;
    sexpr_t *expr = NULL;
    for (i = 0; i < size; ++i) {
	v = read_tokens(exprs[i]);
	vector_print(v);

	expr = parse_sexpr(v);

	puts("ssjjjj");
	sexpr_describe(expr);
	vector_free(v);
    }

    /* memory should be freed using GC
     * but for the moment it is not! */
}
#endif
