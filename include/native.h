#ifndef _SCHMIN_NATIVE
#  define _SCHMIN_NATIVE
#  include "types.h"

struct NATIVE_LAMBDA {
    string_t symbol;
    sexpr_t *(*func) (sexpr_t *);	/* native lambda */
};

bool_t isnative(sexpr_t * operator);

sexpr_t *native_add(sexpr_t * expr);
sexpr_t *native_minus(sexpr_t * expr);
sexpr_t *native_times(sexpr_t * expr);
sexpr_t *native_div(sexpr_t * expr);

#endif				/* _SCHMIN_NATIVE */
