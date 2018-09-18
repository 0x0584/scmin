#ifndef _SCMIN_SCOPE_H
#  define _SCMIN_SCOPE_H

#  include "sexpr.h"
#  include "gc.h"

/* this might be used as HashMap also as Strings too, have a
 * Global String context, since a HashMap has O(1) complexity */
struct BOND {
    string_t key;
    sexpr_t *sexpr;
    /* if a bond was constant, it would be unchangeable */
    bool isconst;
};

struct SCOPE {
    gc_info gci;
    vector_t *bonds;
    scope_t *parent;
};

bond_t *bond_new(string_t key, sexpr_t * expr);
void bond_free(object_t b);
bool bond_cmp(object_t o1, object_t o2);
void bond_describe(object_t b);
bool isbonded(scope_t *s,sexpr_t *);
sexpr_t *resolve_bond(scope_t * s, sexpr_t * expr);
bool bind_lambda_args(scope_t *s,lambda_t *l, sexpr_t *args);

scope_t *scope_init(scope_t * parent);
void scope_describe(object_t s);
scope_t *get_global_scope(void);
void scope_push_bond(scope_t * s, bond_t * b);

#endif				/* _SCMIN_SCOPE_H */
