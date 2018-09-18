/*
 * this would contain some native implementation to functions
 * i.e. built-in into the interpreter
 */
#include "../include/native.h"
#include "../include/sexpr.h"
#include "../include/pair.h"

sexpr_t *native_add(sexpr_t * expr) {
    sexpr_t *result = NULL, *tmp = expr, *value;
    number_t n = 0;

    while (!isnil(tmp)) {
	err_raise(ERR_ARG_TYPE, !isnumber(value = car(tmp)));

	if(err_log())
	    return sexpr_err();

	n += value->n;
	tmp = cdr(tmp);
    }

    result = sexpr_new(T_NUMBER);
    result->n = n;

    return result;
}

sexpr_t *native_minus(sexpr_t * expr) {
    number_t n = 0;
    sexpr_t *result = NULL, *tmp = expr, *value;

    while (!isnil(tmp)) {
	err_raise(ERR_ARG_TYPE, !isnumber(value = car(tmp)));

	if(err_log())
	    return sexpr_err();

	n -= value->n;
	tmp = cdr(tmp);
    }

    result = sexpr_new(T_NUMBER);
    result->n = n;

    return result;
}

sexpr_t *native_times(sexpr_t * expr) {
    number_t n = 1;
    sexpr_t *result = NULL, *tmp = expr, *value;

    while (!isnil(tmp)) {
	err_raise(ERR_ARG_TYPE, !isnumber(value = car(tmp)));

	if(err_log())
	    return sexpr_err();

	n *= value->n;
	tmp = cdr(tmp);
    }

    result = sexpr_new(T_NUMBER);
    result->n = n;

    return result;
}

sexpr_t *native_divid(sexpr_t * expr) {
    sexpr_t *result = NULL, *tmp, *tmp0;

    err_raise(ERR_ARG_COUNT, sexpr_length(expr) > 2);
    err_raise(ERR_ARG_TYPE, !isnumber(tmp = car(expr)));
    err_raise(ERR_ARG_TYPE, !isnumber(tmp0 = car(expr)));

    if (err_log())
	return sexpr_err();

    err_raise(ERR_ARG_TYPE, !tmp0->n);

    if (err_log())
	return sexpr_err();

    result = sexpr_new(T_NUMBER);
    result->n = tmp->n / tmp0->n;;

    return result;
}

/* (or s-exprs) */
sexpr_t *native_or(sexpr_t * expr) {
    sexpr_t *ret = NULL, *tmp = expr;

    while (true) {
	if (isnil(tmp)) return tmp;
	else tmp = cdr(tmp);
    }

    return ret;
}

/* (not s-expr) */
sexpr_t *native_not(sexpr_t * expr) {
    return expr;
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
