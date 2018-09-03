#include "../include/scope.h"
#include "../include/native.h"
#include "../include/vector.h"

bond_t *bond_new(string_t key, sexpr_t * expr) {
    if (key == NULL) {
	puts("KEY IS NULL");
	exit(0);
    }
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

bool bond_cmp(object_t o1, object_t key) {
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
    assert(issymbol(expr) || islambda(expr));

    bond_t *resolved = NULL;

    if (!(resolved = vector_find(s->bonds, expr->s))) {
	puts("SYMBOLE COULD NOT BE RESOLVED!");
    }

    return resolved ? resolved->sexpr : NULL;
}

bool isbonded(scope_t * s, sexpr_t * expr) {
    return resolve_bond(s, expr) != NULL;
}

bool bind_lambda_args(scope_t * s, lambda_t * l, sexpr_t * args) {
    return s && l && args;
}

scope_t *scope_init(scope_t * parent) {
    scope_t *s = gc_alloc_scope();
    s->parent = parent;
    return s;
}

scope_t *global_scope_init(void) {
    static native_t stdlib[] = {
	{"+", native_add},
	{"-", native_minus},
	{"*", native_times},
	{"/", native_div},

	{"quote", native_quote},

	{"cons", native_cons},
	{"car", native_car},
	{"cdr", native_cdr},

	{"eq?", native_iseq},
	{"atom?", native_isatom},
	{"pair?", native_ispair},

	{"and", native_and},
	{"or", native_or},
	{"not", native_not},

	{NULL, NULL}
    };

    static int i;
    scope_t *global = scope_init(NULL);

    for (i = 0; stdlib[i].symbol; ++i) {
	native_t *tmp = &stdlib[i];
	sexpr_t *lambda = lambda_new_native(global, NULL, tmp);
	vector_push(global->bonds, bond_new(tmp->symbol, lambda));
    }

    vector_compact(global->bonds);

    return global;
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
