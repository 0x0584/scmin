#ifndef _SCMIN_SCOPE_H
#  define _SCMIN_SCOPE_H

#  include "sexpr.h"
#  include "gc.h"

/* this might be used as HashMap also as Strings too, have a
 * Global String context, since a HashMap has O(1) complexity */
struct BOND {
    string_t key;
    sexpr_t *sexpr;
};

struct SCOPE {
    gc_info gci;
    vector_t *bonds;
    scope_t *parent;
};

bond_t *bond_init(string_t key, sexpr_t * expr);
void bond_free(object_t b);
void bond_describe(object_t b);

scope_t *scope_init(scope_t * parent);

void scope_push_bond(scope_t * s, bond_t * b);
void scope_describe(object_t s);

#endif				/* _SCMIN_SCOPE_H */
