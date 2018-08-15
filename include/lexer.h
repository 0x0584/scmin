#ifndef _SCMIN_LEXER_H
#  define _SCMIN_LEXER_H

/**
 * @file lexer.h
 *
 * this file contains declarations of funtionalities to verify the grammar
 * of a Scheme-like code and get all tokens in there and then using read_tokes()
 * or read_stream_tokens(), it collects the tokens in a into a Vector
 *
 * @see @file token.h
 * @see @file vector.h
 *
 * TODO:
 *	  stop the process somehow!
 */

#  include "main.h"
#  include "token.h"
#  include "characters.h"

/**
 * read and record all the tokens that are in @p code in a Vector
 * by calling next_token() to get each one
 *
 * @see @file vector.h
 * @see @file token.h
 *
 * @return a vector of tokens found in @p code
 */
vector_t *read_tokens(string_t code);

/**
 * read and record all the tokens that are coming from @p stream
 * by first calling stream_as_string() to get the code
 * then, it calls read_tokens() to get each one in a Vector
 *
 * TODO: implement stream_as_string() and then this one
 *
 * @see @file vector.h
 * @see @file token.h
 * @see @file characters.h
 *
 * @return a vector of tokens found in @p code
 */
vector_t *read_stream_tokens(FILE * stream);

/**
 * @berief move through all the tokens in a giving @p code.
 *
 * first, it calls clean_comments() and clean_whitespaces() to
 * clean the @p code. after that, using getnc() to keep track on
 * the stream, it gets a character. it calles determine_token_type()
 * to determine the type of the next token. the it calls one of the
 * read functions based on the result.
 *
 * @return a Token
 *
 * @see getnc() in @file characters.c
 * @see @file token.h
 *
 * @note this function modifies the static values in of getnc()
 */
token_t *next_token(char *code);


/**
 * takes any lisp comments away, it reads characters
 * using getnc()
 *
 * @param code a Scheme-like syntax
 *
 * @return false if it reatches EOF
 *
 * @see getnc() in @file characters.c
 * @note this function modifies the static values in of getnc()
 */
bool_t clean_comments(string_t code);

/**
 * takes any whitespaces away, it reads characters
 * using getnc()
 *
 * @param code a Scheme-like syntax
 *
 * @return false if it reatches EOF
 *
 * @see getnc() in @file characters.c
 * @note this function modifies the static values in of getnc()
 */
bool_t clean_whitespaces(string_t code);

/**
 * reads the token value from @p code as string
 *
 * @param code a Scheme-like syntax
 *
 * @return value of the token if type maches, or NULL otherwise
 *
 * @see @file token.h
 * @see getnc() in @file characters.c
 *
 * @note this function modifies the static values in of getnc()
 */
string_t read_as_string(string_t code);

/**
 * reads the token value from @p code as number
 *
 * @param code a Scheme-like syntax
 *
 * @return value of the token if type maches, or NULL otherwise
 *
 * @see @file token.h
 * @see getnc() in @file characters.c
 *
 * @note this function modifies the static values in of getnc()
 */
string_t read_as_number(string_t code);

/**
 * reads the token value from @p code as number
 *
 * @param code a Scheme-like syntax
 *
 * @return value of the token if type maches, or NULL otherwise
 *
 * @see @file token.h
 * @see getnc() in @file characters.c
 *
 * @note this function modifies the static values in of getnc()
 */
string_t read_as_atom(string_t code);

string_t read_as_lambda(const string_t code);

#  if LEXER_DEBUG == DBG_ON
void lexer_testing(void);
#  endif

#endif				/* _SCMIN_LEXER_H */
