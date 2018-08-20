#ifndef _SCMIN_SEXPR_H
#  define _SCMIN_SEXPR_H
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
    T_PAIR,
    T_NIL, T_ERR
};

struct PAIR {
    sexpr_t *car;		/* data */
    sexpr_t *cdr;		/* next */
};

/**
 * A Lisp token sexpr must contain it's type and then one of
 *   the possible sexprs based on that type
 */
struct S_EXPR {
    gc_info gci;
    type_t type;		/** which TYPE this is */

    /* possible sexprs */
    union {
	string_t s;		/** STRING - ATOM */
	bool_t b;		/** BOOLEAN */
	number_t n;		/** NUMBER */
	pair_t *c;		/** car - cdr */
    } v;
};

sexpr_t *sexpr_new(type_t type);

#endif				/* _SCMIN_SEXPR_H */