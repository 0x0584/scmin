#include "../include/pair.h"
#include "../include/sexpr.h"

/* ==========================================================================
 * NOTES ON CONS: THIS IS WHAT THIS IS SUPPOSED TO SOLVE
 * ==========================================================================
 * + it car and cdr are both atoms, gives us just a pair_t
 * + nil is an empty list == '()
 * + (cons 'a '()) == (list a) lists end with an empry list '()
 * + (cons '() 'a) == same as pair(nil, a) because lists ended with nil
 * +
 * ==========================================================================
 */
sexpr_t *cons(sexpr_t * car, sexpr_t * cdr) {
    assert(car != NULL);
    assert(cdr != NULL);

    sexpr_t *expr = sexpr_new(T_PAIR);
    pair_t *pair = malloc(sizeof *pair);

    pair->car = car;
    pair->cdr = cdr;

    if (isnil(cdr)) {
	pair->islist = true;
    } else if (ispair(cdr)) {
	pair->islist = cdr->v.c->islist;
    } else {
	pair->islist = false;
    }

    expr->v.c = pair;

    return expr;
}

sexpr_t *car(sexpr_t * expr) {
    assert(expr != NULL);

    return ispair(expr) ? expr->v.c->car : NULL;
}

sexpr_t *cdr(sexpr_t * expr) {
    return ispair(expr) ? expr->v.c->cdr : NULL;
}

void set_cdr(sexpr_t * expr, sexpr_t * cdr) {
    if (ispair(expr)) {
	expr->v.c->cdr = cdr;
    }
}

void set_car(sexpr_t * expr, sexpr_t * car) {
    if (ispair(expr)) {
	expr->v.c->car = car;
    }
}
