#ifndef _SCMIN_LEXER_H
#  define _SCMIN_LEXER_H

/**
 * @file lexer.h
 *
 * this file contains declarations of functionalities to verify the grammar
 * of a Scheme-like code and get all tokens in there and then using
 * read_tokes() or read_stream_tokens(), it collects the tokens in a into
 * a Vector
 *
 * @see token.h
 * @see vector.h
 */

#  include "main.h"
#  include "token.h"
#  include "chars.h"

vector_t *read_tokens(string_t code);
vector_t *read_stream_tokens(const string_t filename);
token_t *read_next_token(string_t code);
string_t read_string(string_t code);
string_t read_number(string_t code);
string_t read_symbol(string_t code);

void lexer_testing(void);

#endif				/* _SCMIN_LEXER_H */
