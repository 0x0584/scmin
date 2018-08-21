#include "sexpr.h"

struct PAIR {
    sexpr_t *car;		/* data */
    sexpr_t *cdr;		/* next */
};

pair_t *cons(sexpr_t *car, sexpr_t *cdr);
void set_cdr(sexpr_t *old_cdr, sexpr_t *new_cdr);
