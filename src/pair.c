#include "../include/pair.h"
#include "../include/sexpr.h"

/* ==========================================================================
 * NOTES ON CONS: THIS IS WHAT THIS IS SUPPOSED TO SOLVE
 * ==========================================================================
 * + it car and cdr are both symbols, gives us just a pair_t
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

    expr->c = malloc(sizeof(pair_t));
    expr->c->car = car;
    expr->c->cdr = cdr;

    if (isnil(cdr)) {
	expr->c->islist = true;
    } else if (ispair(cdr)) {
	expr->c->islist = cdr->c->islist;
    } else {
	expr->c->islist = false;
    }


    return expr;
}

sexpr_t *car(sexpr_t * expr) {
    assert(expr != NULL);

    return ispair(expr) ? expr->c->car : NULL;
}

sexpr_t *cdr(sexpr_t * expr) {
    assert(expr != NULL);

    if (expr->c->cdr->type == T_NIL) {
	return expr->c->cdr;
    }

    assert(expr->type != T_NIL);

    return ispair(expr) ? expr->c->cdr : NULL;
}

void set_cdr(sexpr_t * expr, sexpr_t * cdr) {
    if (ispair(expr)) {
	expr->c->cdr = cdr;
    }
}

void set_car(sexpr_t * expr, sexpr_t * car) {
    if (ispair(expr)) {
	expr->c->car = car;
    }
}
