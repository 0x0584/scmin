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


bool_t isnil(sexpr_t *expr);
bool_t isatom(sexpr_t *expr);
bool_t isnumber(sexpr_t *expr);
bool_t isstring(sexpr_t *expr);
bool_t isboolean(sexpr_t *expr);
bool_t ispair(sexpr_t *expr);

sexpr_t *sexpr_new(type_t type);
void sexpr_describe(sexpr_t * expr);


#endif				/* _SCMIN_SEXPR_H */
