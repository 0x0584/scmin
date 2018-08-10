#ifndef _SCMIN_REPL_H
#  define _SCMIN_REPL_H
#  include "main.h"
#  include "gc.h"

struct KEY_VALUE {
    char *key;
    value_t *value;
};

struct SCOPE {
    gc_info info;
    int limit, nargs;
    kv_t *args;
    scope_t *parent;
};

struct CONTEXT {
    scope_t *scope;
    value_t *expr, *childresult;
};

#endif				/* _SCMIN_REPL_H */
