#ifndef _SCMIN_SCOPE_H
#  define _SCMIN_SCOPE_H

#  include "sexpr.h"
#  include "gc.h"

/**
 * @brief a symbol that would be related to a s-expression
 *
 * when evaluating a symbol who's like `symbol`, `sexpr` is returned
 * instead. this is done using resolve_bond()
 *
 * @note this might be used as `HashMap` also as Strings too, have a
 * Global String context, since a `HashMap` has `O(1)` complexity
 */
typedef struct SCOPE_BOND {
    /**
     * @brief this is a string that would be represent a symbol
     */
    string_t symbol;

    /**
     * @brief a s-expression
     */
    sexpr_t *sexpr;

    /**
     * @brief if `true` the bond would be unchangeable
     *
     * @todo implement this
     * @warning **not implemented!**
     */
    bool isconst;
} bond_t;

/**
 * @brief a scope is a environment where the evaluation gets its
 * predefined expression (i.e. bonds)
 *
 * @note the global scope has no parent `(parent == NULL)`
 */
typedef struct SCOPE {
    /**
     * @details Garbage Collection information
     */
    gc_info gci;

    /**
     * @brief a vector of scope's bonds
     * @see #VECTOR
     */
    vector_t *bonds;

    /**
     * @brief a scope may have a parent or not
     */
    scope_t *parent;
} scope_t;

bond_t *bond_new(string_t key, sexpr_t * expr);
void bond_free(object_t b);
bool bond_cmp(object_t o1, object_t o2);
void bond_describe(object_t b);
bool isbonded(scope_t *s,sexpr_t *);
bond_t *resolve_bond(scope_t * s, sexpr_t * expr);
void bind_lambda_args(scope_t *s,lambda_t *l, sexpr_t *args);
bool isreserved(sexpr_t *expr);

scope_t *scope_init(scope_t * parent);
void scope_describe(object_t s);
scope_t *get_global_scope(void);
void scope_push_bond(scope_t * s, bond_t * b);
void setglobal(sexpr_t * sexpr, bool isglobal);

#endif				/* _SCMIN_SCOPE_H */
