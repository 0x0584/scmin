#ifndef _SCMIN_TOKEN_H
#  define _SCMIN_TOKEN_H

/**
 * @file token.h
 *
 * @brief definition of functionalities to handle tokens and their types
 */

#  include "main.h"

/* ================ macros ================ */

/**
 * @brief limit of token characters
 */
#  define TOK_SIZE_LIMIT	(2 << 8)

/**
 * @brief a macro to test if the #TOK_SIZE_LIMIT was reached
 */
#  define TOK_LIMIT(index)	(index > TOK_SIZE_LIMIT - 1)

/* ================ datat types ================ */

/**
 * @brief set of token types
 */
typedef enum TOKEN_TYPE {
    /**
     * @brief left parenthesis `(`
     */
    TOK_L_PAREN = 0,

    /**
     * @brief right parenthesis `)`
     */
    TOK_R_PAREN,

    /**
     * @brief quote `'`
     */
    TOK_QUOTE,

    /**
     * @brief anything between two `"`
     */
    TOK_STRING,

    /**
     * @brief any number like `1412`
     */
    TOK_NUMBER,

    /**
     * @brief anything else such as `foo` or `string?`
     */
    TOK_SYMBOL,

    /**
     * @brief flag a lexing issue
     */
    TOK_ERR,

    /**
     * @brief END OF LEXING
     */
    EOL
} token_type;

/**
 * @brief a token is composed from a type and a value stored in the buffer
 * with a depth in the s-expr
 */
typedef struct TOKEN {
    /**
     * @brief the token type
     */
    token_type type;

    /**
     * @brief the content of the token as string
     */
    string_t vbuffer;

    /**
     * @brief depth of the token within the expression
     */
    int depth;
} token_t;

/* ================ function prototypes ================ */

token_type predict_token_type(string_t src);
token_t *token_new(token_type type, string_t vbuffer, int depth);
void token_print(object_t t);
void token_free(object_t o);

#endif				/* _SCMIN_TOKEN_H */
