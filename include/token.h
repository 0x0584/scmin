#ifndef _SCMIN_TOKEN_H
#  define _SCMIN_TOKEN_H

/**
 * @file token.h
 *
 * this file contains definition of functionalities to handle the token
 * by defining the possible types of a token. and also indicating an error
 * while lexing using token type flags
 */

#  include "main.h"

#  define TOK_SIZE_LIMIT	(2 << 8)
#  define TOK_LIMIT(index)	(index > TOK_SIZE_LIMIT - 1)

/**
 * set of token types
 */
enum TOKEN_TYPE {
    TOK_L_PAREN = 0,
    TOK_R_PAREN,

    TOK_QUOTE,

    TOK_SYMBOL,
    TOK_STRING,
    TOK_NUMBER,

    TOK_ERR,
    EOL
} type;

/**
 * a token is composed from a type and a value stored in the buffer
 * with a depth in the s-expr
 */
struct TOKEN {
    token_type type;
    string_t vbuffer;
    int depth;
};

/**
 * takes a character which represent the beginning of a token
 * and then it determines it's type
 *
 * @param c a character
 *
 * @return a token type or TOK_ERR if no type matches
 */
token_type predict_token_type(string_t src);

/**
 * create a token
 *
 * @param type a token type
 * @param vbuffer the value of the token
 * @param depth the depth of the token
 *
 * @return the token
 */
token_t *token_new(token_type type, string_t vbuffer, int depth);

/**
 * print the token of @p t on the screen
 *
 * @param t a token
 */
void token_print(object_t t);

/**
 * free token @p t and
 *
 * @param t a token
 */
void token_free(object_t o);



#endif				/* _SCMIN_TOKEN_H */
