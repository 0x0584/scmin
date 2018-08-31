#include "../include/scope.h"
#include "../include/vector.h"

bond_t *bond_new(string_t key, sexpr_t * expr) {
    assert(key != NULL);

    bond_t *b = malloc(sizeof *b);

    b->key = key;
    b->sexpr = expr;

    return b;
}

void bond_free(object_t o) {
    if (o == NULL)
	return;

    bond_t *b = o;

    free(b->key);
    free(b);
    /* b->sexpr is handled by the garbage collector! */
}
bool_t bond_cmp(object_t o1, object_t key) {
    assert(o1 != NULL);
    assert(key != NULL);

    bond_t *b1 = o1;

    return !strcmp(b1->key, (string_t) key);
}

void bond_describe(object_t o) {
    assert(o != NULL);

    if (o == NULL)
	return;

    bond_t *b = o;

    printf("key: %s\n", b->key);
    printf("sexpr:\n");
    sexpr_describe(b->sexpr);
}

sexpr_t *resolve_bond(scope_t * s, sexpr_t * expr) {
    assert(issymbol(expr));

    bond_t *resolved = NULL;

    if (!(resolved = vector_find(s->bonds, expr->s))) {
	puts("SYMBOLE COULD NOT BE RESOLVED!");
    }

    return resolved ? resolved->sexpr : NULL;
}

bool_t isbonded(scope_t * s, sexpr_t * expr) {
    return resolve_bond(s, expr) != NULL;
}

bool_t bind_lambda_args(scope_t * s, lambda_t * l, sexpr_t * args) {
    return s && l && args;
}

scope_t *scope_init(scope_t * parent) {
    scope_t *s = gc_alloc_scope();
    s->parent = parent;
    return s;
}

void scope_describe(object_t o) {
    if (o == NULL) {
	puts("scope was NULL");
	return;
    }

    scope_t *s = o;

    printf("is marked? [%s]\n", s->gci.ismarked ? "X" : " ");
    vector_debug(stdout, s->bonds);

    if (s->parent) {
	puts(" parent");
	scope_describe(s->parent);
    }
}

void scope_push_bond(scope_t * s, bond_t * b) {
    bond_t *tmp = vector_find(s->bonds, b);

    if (!tmp) {
	vector_push(s->bonds, b);
    } else {
	tmp->sexpr = b->sexpr;
	bond_free(b);
    }
}
