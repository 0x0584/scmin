#ifndef _SCMIN_SEXPR_H
#  define _SCMIN_SEXPR_H

#  include "main.h"
#  include "gc.h"

/**
 * Possible types for an expression to be
 */
enum S_EXPR_TYPE {
    T_PAIR,			/** car, cdr */
    T_NUMBER,			/** 0 -100 0.25 */
    T_STRING,			/** "string" */
    T_SYMBOL,			/** foo foo-bar */
    T_NIL, T_ERR
};


/**
 * the lambda expression is an expression that takes
 * expressions as arguments, i.e. a function
 *
 * (lambda (args) s-exprs)
 */
struct LAMBDA {
    sexpr_t *args;		/* cadr */
    bool_t isnative;

    union {
	Nlambda_t *l;		/* native lambda */
	sexpr_t *body;		/* cddr */
    };
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
	number_t n;		/** NUMBER */
	pair_t *c;		/** car - cdr */
	lambda_t *l;		/* lambda expression */
    };
};

bool_t isnil(sexpr_t * expr);
bool_t isatom(sexpr_t * expr);
bool_t isnumber(sexpr_t * expr);
bool_t isstring(sexpr_t * expr);
bool_t isboolean(sexpr_t * expr);
bool_t ispair(sexpr_t * expr);

sexpr_t *sexpr_new(type_t type);
void sexpr_describe(object_t expr);

#endif				/* _SCMIN_SEXPR_H */
