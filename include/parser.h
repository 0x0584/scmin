#ifndef _SCMIN_PARSER_H
#  define _SCMIN_PARSER_H
#  include "main.h"
#  include "gc.h"

/**
 * Possible types for an expression to be
 */
enum TYPE {
    T_NUMBER,		/** 0 -100 0.25 */
    T_BOOLEAN,		/** #t #f t nil */
    T_STRING,		/** "anything in between" */
    T_ATOM,		/** foo foo-bar */
    T_LAMBDA,		/** anything after lambda  */
    T_PAIR,
    T_NIL, T_ERR
};

struct PAIR {
    sexpr_t *car, *cdr;
};

/**
 * The famous Lambda Expression
 */
struct LAMBDA {
    gc_info info;
    sexpr_t *args;		/** lambda's arguments */
    bool_t isnative;		/** is it built-in? */

    union {
	native_t f;		/** if it was built-in */
	sexpr_t *b;		/** if it was interpreted */
    };
};

/**
 * A Lisp token sexpr must contain it's type and then one of
 *   the possible sexprs based on that type
 */
struct S_EXPR {
    gc_info info;
    type_t type;		/** which TYPE this is */

    /* possible sexprs */
    union {
	string_t s;		/** STRING - ATOM */
	bool_t b;		/** BOOLEAN */
	number_t n;		/** NUMBER */
	lambda_t *l;		/** LAMBDA */
	pair_t *c;		/** car - cdr */
    } v;
};

#endif				/* _SCMIN_PARSER_H */
