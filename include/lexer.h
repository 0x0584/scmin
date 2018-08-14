#ifndef _SCMIN_LEXER_H
#  define _SCMIN_LEXER_H
#  include "main.h"
#  include "gc.h"
#  include "token.h"
#  define LEXER_DEBUG

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
