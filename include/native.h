#ifndef _SCHMIN_NATIVE
#  define _SCHMIN_NATIVE

#  include "types.h"
#  include "main.h"

/**
 *
 */
typedef struct NATIVE_LAMBDA {
    string_t symbol;
    sexpr_t *(*func) (sexpr_t *);	/* native lambda */
} native_t;

sexpr_t *native_add(sexpr_t * expr);
sexpr_t *native_minus(sexpr_t * expr);
sexpr_t *native_times(sexpr_t * expr);
sexpr_t *native_divid(sexpr_t * expr);
sexpr_t *native_eq(sexpr_t * expr);
sexpr_t *native_less(sexpr_t * expr);
sexpr_t *native_greater(sexpr_t * expr);
sexpr_t *native_less_eq(sexpr_t * expr);
sexpr_t *native_greater_eq(sexpr_t * expr);
sexpr_t *native_sqrt(sexpr_t * expr);
sexpr_t *native_square(sexpr_t * expr);

/* (or sexprs) */
sexpr_t *native_or(sexpr_t * expr);
/* (not sexpr) */
sexpr_t *native_not(sexpr_t * expr);
/* (and sexprs) */
sexpr_t *native_and(sexpr_t * expr);
/* (pair? sexpr) */
sexpr_t *native_ispair(sexpr_t * expr);
/* (eq? sexprs) */
sexpr_t *native_iseq(sexpr_t * expr);
/* (atom? sexpr) */
sexpr_t *native_isatom(sexpr_t * expr);

sexpr_t *native_list(sexpr_t * expr);
sexpr_t *native_length(sexpr_t * expr);

/* (cons sexpr0 sexpr1) */
sexpr_t *native_cons(sexpr_t * expr);
/* (car sexpr) */
sexpr_t *native_car(sexpr_t * expr);
/* (cdr sexpr) */
sexpr_t *native_cdr(sexpr_t * expr);
sexpr_t *native_set_car(sexpr_t * expr);
sexpr_t *native_set_cdr(sexpr_t * expr);

sexpr_t *native_print(sexpr_t * expr);

sexpr_t *native_isnil(sexpr_t * expr);
sexpr_t *native_istrue(sexpr_t * expr);
sexpr_t *native_isstring(sexpr_t * expr);
sexpr_t *native_isnumber(sexpr_t * expr);
sexpr_t *native_issymbol(sexpr_t * expr);
sexpr_t *native_islambda(sexpr_t * expr);
sexpr_t *native_islist(sexpr_t * expr);


#endif				/* _SCHMIN_NATIVE */
