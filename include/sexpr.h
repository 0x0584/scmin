#ifndef _SCMIN_SEXPR_H
#  define _SCMIN_SEXPR_H

#  include "main.h"
#  include "gc.h"

/**
 * Possible types for an expression to be
 */
enum S_EXPR_TYPE {
    T_PAIR,			/* car, cdr */
    T_NUMBER,			/* 0 -100 0.25 */
    T_STRING,			/* "string" */
    T_SYMBOL,			/* foo foo-bar */
    T_LAMBDA,			/* (lambda (args) ...) */

    T_NIL,			/* like NULL */
    T_ERR			/* ERROR */
};

/**
 * the lambda expression is an expression that takes
 * expressions as arguments, i.e. a function
 *
 * (lambda (args) s-exprs)
 */
struct LAMBDA {
    gc_info gci;
    scope_t *parent;
    sexpr_t *args;		/* cadr */
    bool isnative;

    union {
	native_t *native;	/* native lambda */
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

bool isnil(sexpr_t * expr);
bool isatom(sexpr_t * expr);
bool isnumber(sexpr_t * expr);
bool isstring(sexpr_t * expr);
bool issymbol(sexpr_t * expr);
bool islambda(sexpr_t * expr);
bool ispair(sexpr_t * expr);

sexpr_t *sexpr_new(type_t type);
void sexpr_describe(object_t expr);
void sexpr_print(object_t expr);
sexpr_t *lambda_new_native(scope_t * parent, sexpr_t * args,
			   native_t * func);
sexpr_t *lambda_new(scope_t * parent, sexpr_t * args, sexpr_t * body);
void lambda_describe(object_t expr);

#endif				/* _SCMIN_SEXPR_H */
