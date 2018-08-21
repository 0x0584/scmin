#include "../include/pair.h"

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
pair_t *cons(sexpr_t *car, sexpr_t *cdr) {
    pair_t *p = malloc(sizeof *p);

    p->car = car;
    p->cdr = cdr;

    return p;
}

void set_cdr(sexpr_t *old_cdr, sexpr_t *new_cdr);
