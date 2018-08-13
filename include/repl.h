#ifndef _SCMIN_REPL_H
#  define _SCMIN_REPL_H
#  include "main.h"
#  include "gc.h"

struct DATA {
    char *key;
    sexpr_t *sexpr;
};

struct SCOPE {
    gc_info info;
    int limit, nargs;
    data_t *args;
    scope_t *parent;
};

struct CONTEXT {
    scope_t *scope;
    sexpr_t *expr, *childresult;
};

#endif				/* _SCMIN_REPL_H */
