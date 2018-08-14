#ifndef _SCMIN_TOKEN_H
#  define _SCMIN_TOKEN_H
#  include "main.h"

enum TOKEN_TYPE {
    TOK_L_PAREN = 0, TOK_R_PAREN,
    TOK_S_QUOTE, TOK_D_QUOTE,
    TOK_LAMBDA, TOK_ATOM, TOK_NUMBER, TOK_STRING,
    TOK_EOF = (-1), TOK_ERR = (-2),
    TOK_SIZE_LIMIT = (2 << 8)
} type;

struct TOKEN {
    token_type type;
    string_t buffer;
    int depth;
};

token_t *token_new(token_type type, string_t str, int depth);
void token_print(object_t t);
void token_free(object_t o);

#endif				/* _SCMIN_TOKEN_H */
