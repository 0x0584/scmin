#ifndef _SCMIN_LEXER_H
#  define _SCMIN_LEXER_H
#  include "main.h"
#  include "gc.h"

struct TOKEN {
    enum {
	TOK_L_PAREN = 0, TOK_R_PAREN, TOK_PERIOD,
	TOK_S_QUOTE, TOK_D_QUOTE,
	TOK_ATOM, TOK_STRING,
	TOK_EOF = (-1), TOK_ERR = (-2),
	TOK_LIMIT = (2<<7)
    } type;
    unsigned char buffer[TOK_LIMIT];
};

vector_t *read_tokens(string_t);
string_t stream_as_string(FILE *);
vector_t *read_stream_tokens(FILE *);

#endif				/* _SCMIN_LEXER_H */
