#ifndef _SCMIN_LEXER_H
#  define _SCMIN_LEXER_H
#  include "main.h"
#  include "gc.h"

#  define LEXER_DEBUG

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

token_t *token_new(token_type, string_t, int);
void token_free(object_t);
void token_print(object_t);

token_t *next_token(char *code, int depth);
vector_t *read_tokens(string_t);
string_t stream_as_string(FILE *);
vector_t *read_stream_tokens(FILE *);

string_t read_string(string_t code);
string_t read_number(string_t code);
string_t read_atom(string_t code);

#  if defined LEXER_DEBUG
void lexer_testing(void);
#  endif

#endif				/* _SCMIN_LEXER_H */
