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

/**
 * set of token types
 */
enum TOKEN_TYPE {
    TOK_L_PAREN = 0,		/* ( */
    TOK_R_PAREN,		/* ) */
    TOK_S_QUOTE,		/* (quote foo) */
    TOK_D_QUOTE,		/* "string" */
    TOK_LAMBDA,			/* lambda (params, ..) s-exprs.. */
    TOK_ATOM,			/* bar */
    TOK_NUMBER,			/* .3 -3.1415  */

    /* flags */
    TOK_EOL = (-1),		/* end of lexing */
    TOK_ERR = (-2),		/* lexing error */
    TOK_SIZE_LIMIT = (2 << 8)
} type;

/**
 * a token bas a type and a value stored in the buffer
 * with a depth in teh s-expr
 */
struct TOKEN {
    token_type type;
    string_t vbuffer;
    int depth;
};

/**
 * takes a charcater which represent the beginning of a token
 * and then it determines it's type
 *
 * @param c a character
 *
 * @return a token type or TOK_ERR if no type matches
 */
token_type predict_token_type(char c);

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
 * print the token of @p t on the screan
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
