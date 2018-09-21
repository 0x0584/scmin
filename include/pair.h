#ifndef _SCMIN_PAIR_H
#  define _SCMIN_PAIR_H

#  include "main.h"
#  include "gc.h"

struct PAIR {
    sexpr_t *car;		/* current */
    sexpr_t *cdr;		/* rest */
    bool islist, ishead;
};

sexpr_t *cons(sexpr_t * car, sexpr_t * cdr);
sexpr_t *car(sexpr_t * expr);
sexpr_t *cdr(sexpr_t * expr);

sexpr_t *caar(sexpr_t * expr);
sexpr_t *cadr(sexpr_t * expr);
sexpr_t *cddr(sexpr_t * expr);
sexpr_t *cdar(sexpr_t * expr);
sexpr_t *caddr(sexpr_t * expr);
void set_cdr(sexpr_t * expr, sexpr_t * cdr);
void set_car(sexpr_t * expr, sexpr_t * car);

#endif
