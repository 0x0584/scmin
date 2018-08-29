#include "../include/scope.h"
#include "../include/vector.h"

bond_t *bond_init(string_t key, sexpr_t *expr) {
    bond_t *b = malloc(sizeof *b);

    b->key = key;
    b->sexpr = expr;

    return b;
}

void bond_free(object_t o) {
    if(o == NULL) return;

    bond_t *b = o;

    free(b->key);
    /* b->sexpr is handled by the garbage collector! */
}

void bond_describe(object_t b) {
    if (b == NULL)  return;
}

void scope_push_bond(scope_t *s, bond_t *b) {
    bond_t *tmp = vector_find(s->bonds, b);

    if (!tmp) {
	vector_push(s->bonds, b);
    } else {
	tmp->sexpr  = b->sexpr;
	bond_free(b);
    }
}

scope_t *scope_init(scope_t *parent) {
    scope_t *s = gc_alloc_scope();

    s->parent = parent;

    return s;
}

void scope_describe(object_t o) {
    assert(o != NULL);

    scope_t *s = o;

    printf("[%s]\n", s->gci.ismarked ? "X" : " ");
    vector_debug(stdout, s->bonds);

    if (s->parent) {
	puts(" parent");
	scope_describe(s->parent);
    }
}
