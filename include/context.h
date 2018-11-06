#ifndef _SCMIN_CONTEXT_H
#  define _SCMIN_CONTEXT_H

#  include "sexpr.h"

typedef struct CONTEXT {
    gc_info gci;
    scope_t *scope;
    sexpr_t *sexpr, *childresult;
    vector_t *reg;
} context_t;

context_t *context_init(scope_t * s, sexpr_t * expr);
void context_reset(context_t c, scope_t * s, sexpr_t * expr);
context_t *context_current(void);
void context_add_local(context_t * c, sexpr_t * expr);
void context_describe(object_t o);
#endif				/*  _SCMIN_CONTEXT_H */
