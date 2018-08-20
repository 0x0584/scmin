/*
 * this would take a set of tokens and then creates a parse-tree
 * PS: or something like that
 */

#include "../include/parser.h"

#include "../include/sexpr.h"
#include "../include/token.h"
#include "../include/vector.h"

sexpr_t *parse_expression(vector_t *tokens) {
    int i;
    sexpr_t *expr = NULL;
    token_t *token = NULL;

    for (i = 0; i < tokens->size; ++i) {
	token = vector_get(tokens, i);

	switch (token->type) {
	case TOK_L_PAREN:
	    expr = parse_as_list(tokens, &i);
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
    }

    assert(expr != NULL);

    return expr;
}


sexpr_t *parse_as_number(string_t value) {
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

    raise_error(error[noerror]);
    return NULL;
}

sexpr_t *parse_as_string(string_t value) {
    return NULL;
}
sexpr_t *parse_as_atom(string_t value) {
    return NULL;
}
sexpr_t *parse_as_boolean(string_t value) {
    return NULL;
}

sexpr_t *parse_as_list(vector_t *v, int *start) {
    string_t error[] = {
	"LIST HAS NO CLOSE PAREN"
    };
    int i = 0, noerror = 0;
    bool_t isfinished = false;
    sexpr_t *expr = sexpr_new(T_PAIR);
    token_t *token = NULL;

    for (i = *start + 1; i < v->size; ++i) {
	token = v->objs[i];

	if (token->type == TOK_R_PAREN) {
	    isfinished = true;
	    break;
	}
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
