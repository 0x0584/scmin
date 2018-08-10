#ifndef _SCMIN_LEXER_H
#  define _SCMIN_LEXER_H
#  include "main.h"
#  include "gc.h"

/*! Possible types for an expression to be */
enum TOKEN_TYPE {
    TOKEN_NUMBER,		/*! 0 -100 0.25 */
    TOKEN_BOOLEAN,		/*! #t #f t nil */
    TOKEN_STRING,		/*! "anything in between" */
    TOKEN_ATOM,			/*! foo foo-bar */
    TOKEN_LAMBDA,		/*! anything after lambda  */
    TOKEN_NIL, TOKEN_ERROR
};

/*! The famous Lambda Expression */
struct LAMBDA {
    gc_info info;
    value_t *args;		/*! lambda's arguments */
    bool_t isnative;		/*! is it built-in? */

    union {
	native_t f;		/*! if it was built-in */
	value_t *b;		/*! if it was interpreted */
    };
};

/*!
 * A Lisp token value must contain it's type and then one of
 *   the possible values based on that type
 */
struct VALUE {
    gc_info info;
    type_t type;		/*! which TOKEN_TYPE this is */

    /* possible values */
    union {
	string_t s;		/*! TOKEN_STRING - TOKEN_ATOM */
	bool_t b;		/*! TOKEN_BOOLEAN */
	number_t n;		/*! TOKEN_NUMBER */
	lambda_t *l;		/*! TOKEN_LAMBDA */
    } v;
};

#endif				/* _SCMIN_LEXER_H */
