/*
 * this would contain some native implementation to functions
 * i.e. built-in into the interpreter
 */
#include "../include/native.h"
#include "../include/sexpr.h"
#include "../include/pair.h"

bool_t isnative(sexpr_t *operator) {
    return operator != NULL;
}

sexpr_t *native_add(sexpr_t * expr) {
    number_t n = 0;
    sexpr_t *tmp;

    sexpr_t *result = sexpr_new(T_NUMBER);


    while (ispair(expr)) {
	tmp = car(expr);

	n += tmp->n;
	tmp = cdr(tmp);
    }

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
