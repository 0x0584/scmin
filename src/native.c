/*
 * this would contain some native implementation to functions
 * i.e. built-in into the interpreter
 */
#include "../include/native.h"
#include "../include/sexpr.h"
#include "../include/pair.h"

bool isnative(sexpr_t * expr) {
    assert(expr != NULL);
    return expr->l->isnative;
}

sexpr_t *native_add(sexpr_t * expr) {
    sexpr_t *result = NULL, *tmp = expr;
    number_t n = 0;

    while (!isnil(tmp)) {
	sexpr_t *value = car(tmp);
	n += value->n;
	tmp = cdr(tmp);
    }

    result = sexpr_new(T_NUMBER);
    result->n = n;

    return result;
}

sexpr_t *native_minus(sexpr_t * expr) {
    number_t n = 0;
    sexpr_t *tmp;

    sexpr_t *result = sexpr_new(T_NUMBER);


    while (ispair(expr)) {
	tmp = car(expr);

	n -= tmp->n;
	tmp = cdr(tmp);
    }

    result->n = n;

    return result;
}

sexpr_t *native_times(sexpr_t * expr) {
    number_t n = 1;
    sexpr_t *tmp;

    sexpr_t *result = sexpr_new(T_NUMBER);


    while (ispair(expr)) {
	tmp = car(expr);

	n *= tmp->n;
	tmp = cdr(tmp);
    }

    result->n = n;

    return result;
}

sexpr_t *native_div(sexpr_t * expr) {
    number_t n = 1;
    sexpr_t *tmp;

    sexpr_t *result = sexpr_new(T_NUMBER);


    while (ispair(expr)) {
	tmp = car(expr);

	if (!tmp->n) {
	    return NULL;	/* not dividing by zero */
	}

	n /= tmp->n;
	tmp = cdr(tmp);
    }

    result->n = n;

    return result;
}

/* 'expr i.e. (quote expr) */
sexpr_t *native_quote(sexpr_t * expr) {
    return NULL;
}

/* (or s-exprs) */
sexpr_t *native_or(sexpr_t * expr) {
    return NULL;
}

/* (not s-expr) */
sexpr_t *native_not(sexpr_t * expr) {
    return NULL;
}

/* (and s-exprs) */
sexpr_t *native_and(sexpr_t * expr) {
    return NULL;
}

/* (pair? sexpr) */
sexpr_t *native_ispair(sexpr_t * expr) {
    return NULL;
}

/* (eq? sexprs) */
sexpr_t *native_iseq(sexpr_t * expr) {
    return NULL;
}

/* (atom? sexpr) */
sexpr_t *native_isatom(sexpr_t * expr) {
    return NULL;
}

/* (cons sexpr0 sexpr1) */
sexpr_t *native_cons(sexpr_t * expr) {
    return NULL;
}

/* (car sexpr) */
sexpr_t *native_car(sexpr_t * expr) {
    return NULL;
}

/* (cdr sexpr) */
sexpr_t *native_cdr(sexpr_t * expr) {
    return NULL;
}
