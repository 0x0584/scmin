#include "sexpr.h"

struct DATA {
    char *key;
    sexpr_t *sexpr;
};

struct SCOPE {
    gc_info gci;
    int limit, nargs;
    data_t *args;
    scope_t *parent;
};

struct CONTEXT {
    scope_t *scope;
    sexpr_t *expr, *childresult;
    vector_t *locals;
};

context_t *context_init(scope_t *s, sexpr_t *expr);
void context_reset(context_t c, scope_t *s, sexpr_t*expr);
context_t *context_current(void);
void context_add_local(context_t *c, sexpr_t *expr);
