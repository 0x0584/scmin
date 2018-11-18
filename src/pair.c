#include "pair.h"
#include "sexpr.h"

sexpr_t *cons(sexpr_t * car, sexpr_t * cdr) {
    err_raise(ERR_ARG_TYPE, car == NULL);
    err_raise(ERR_ARG_TYPE, cdr == NULL);

    if (err_log())
	return sexpr_err();

    sexpr_t *expr = sexpr_new(LISP_PAIR);

    expr->c = malloc(sizeof(pair_t));
    expr->c->car = car;
    expr->c->cdr = cdr;

    if (isnil(cdr))
	expr->c->islist = true;
    else if (ispair(cdr))
	expr->c->islist = cdr->c->islist;
    else
	expr->c->islist = false;

    return expr;
}

sexpr_t *car(sexpr_t * expr) {
    return ispair(expr) ? expr->c->car : NULL;
}

sexpr_t *cdr(sexpr_t * expr) {
    return ispair(expr) ? expr->c->cdr : NULL;
}

void set_cdr(sexpr_t * expr, sexpr_t * cdr) {
    if (ispair(expr))
	expr->c->cdr = cdr;
}

void set_car(sexpr_t * expr, sexpr_t * car) {
    if (ispair(expr))
	expr->c->car = car;
}

sexpr_t *caar(sexpr_t * expr) {
    return car(car(expr));
}

sexpr_t *cdar(sexpr_t * expr) {
    return cdr(car(expr));
}

sexpr_t *cadr(sexpr_t * expr) {
    return car(cdr(expr));
}

sexpr_t *cddr(sexpr_t * expr) {
    return cdr(cdr(expr));
}

sexpr_t *caddr(sexpr_t * expr) {
    return car(cdr(cdr(expr)));
}

sexpr_t *cdadr(sexpr_t * expr) {
    return cdr(car(cdr(expr)));
}

sexpr_t *cadar(sexpr_t * expr) {
    return car(cdr(car(expr)));
}
