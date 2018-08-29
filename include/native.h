#ifndef _SCHMIN_NATIVE
#  define _SCHMIN_NATIVE
#include "types.h"

struct NATIVE_LAMBDA {
    string_t symbol;
    sexpr_t *(*func) (sexpr_t *);	/* native lambda */
};

#endif				/* _SCHMIN_NATIVE */
