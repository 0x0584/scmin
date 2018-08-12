#ifndef _SCMIN_LEXER_H
#  define _SCMIN_LEXER_H
#  include "main.h"
#  include "gc.h"

#  define LEXER_DEBUG

enum TOKEN_TYPE {
    TOK_L_PAREN = 0, TOK_R_PAREN, TOK_PERIOD,
    TOK_S_QUOTE, TOK_D_QUOTE,
    TOK_ATOM, TOK_STRING,
    TOK_EOF = (-1), TOK_ERR = (-2),
    TOK_SIZE_LIMIT = (2 << 7)
} type;

typedef char token_string[TOK_SIZE_LIMIT];

struct TOKEN {
    token_type type;
    token_string buffer;
    int depth;
};

token_t *token_new(token_type, token_string, int);
void token_free(void *);

vector_t *read_tokens(string_t);
string_t stream_as_string(FILE *);
vector_t *read_stream_tokens(FILE *);

#  if defined LEXER_DEBUG
void lexer_testing(void);
#  endif

#endif				/* _SCMIN_LEXER_H */
