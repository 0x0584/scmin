/**
 * @file scope.c
 *
 * @brief contains definitions of scope functionalities including bonds
 *
 * @todo find a way to remap multiple symbols to the same value, much
 * like pointers
 */

#include "scope.h"
#include "pair.h"
#include "native.h"
#include "vector.h"

/**
 * @brief the global scope of the interpreter
 *
 * @details this global scope cotains predefined lambdas and constants
 * that are essential for the interpreter to function. It's intialized
 * in the first call to get_global_scope()
 */
static scope_t *gs = NULL;

/**
 * @brief this is a remapping of Scheme/Lisp keywords and native C functions
 *
 * @details those functions are impl;emeted in native C because they could
 * not be described in Lisp or Scheme.
 *
 * @see native.c
 * @todo try to replace some of these function using Scheme/Lisp syntax
 */
static native_t stdlib[] = {
    {"+", native_add}, {"add", native_add},
    {"-", native_minus}, {"sub", native_minus},
    {"*", native_times}, {"mul", native_times},
    {"/", native_divid}, {"div", native_divid},

    /* numerical equivalence require numbers */
    {"=", native_eq}, {"eq", native_eq},
    {"<", native_less}, {"less", native_less},
    {">", native_greater}, {"greater", native_greater},
    {"<=", native_less_eq}, {"less-or-eq", native_less_eq},
    {">=", native_greater_eq}, {"greater-or-eq", native_greater_eq},

    {"sqrt", native_sqrt},
    {"square", native_square},

    {"list", native_list},
    {"length", native_length},
    {"cons", native_cons},
    {"car", native_car},
    {"cdr", native_cdr},
    {"set-car", native_set_car},
    {"set-cdr", native_set_cdr},

    /* eq? would return true if only the type matches */
    {"eq?", native_iseq},
    {"nil?", native_isnil},
    {"true?", native_istrue},
    {"string?", native_isstring},
    {"number?", native_isnumber},
    {"symbol?", native_issymbol},
    {"lambda?", native_islambda},
    {"list?", native_islist},
    {"atom?", native_isatom},
    {"pair?", native_ispair},

    {"and", native_and},
    {"or", native_or},
    {"not", native_not},

    {"print", native_print},

    {NULL, NULL}
};

bond_t *bond_new(string_t key, sexpr_t * expr) {
    assert(key != NULL);
    assert(expr != NULL);

    bond_t *b = malloc(sizeof *b);

    b->key = strdup(key);
    b->sexpr = expr;
    b->isconst = false;

    return b;
}

bond_t *bond_new_const(string_t key, sexpr_t * expr) {
    bond_t *b = bond_new(key, expr);
    b->isconst = true;
    return b;
}

void bond_free(object_t o) {
    bond_t *b = o;

    if (b == NULL)
	return;
    else
	free(b->key), free(b);
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
    printf("sexpr: ");
    sexpr_print(b->sexpr);
}

sexpr_t *resolve_bond(scope_t * s, sexpr_t * expr) {
    if (!issymbol(expr))
	return NULL;

    bond_t *resolved = vector_find(s->bonds, expr->s);

    /*
       bond_describe(resolved);
       assert(resolved != NULL);
    */

    if (resolved == NULL && s->parent != NULL)
	resolved = vector_find(s->parent->bonds, expr->s);

    return resolved ? resolved->sexpr : NULL;
}

bool isbonded(scope_t * s, sexpr_t * expr) {
    return resolve_bond(s, expr) != NULL;
}

void bind_lambda_args(scope_t * s, lambda_t * l, sexpr_t * args) {
    sexpr_t *foo = l->args, *bar = NULL;	/* lambda's tmp */
    sexpr_t *fuzz = args, *buzz = NULL;	/* arg's tmp */

    while (!isnil(foo)) {
	bar = car(foo), buzz = car(fuzz);
	vector_push(s->bonds, bond_new(bar->s, buzz));
	foo = cdr(foo), fuzz = cdr(fuzz);
    }
}

scope_t *scope_init(scope_t * parent) {
    scope_t *s = gc_alloc_scope();
    s->parent = parent;
    return s;
}

scope_t *get_global_scope(void) {
    scope_t *global_scope_init(void);	/* private header */

    return global_scope_init();
}

scope_t *global_scope_init(void) {
    if (gs != NULL)
	return gs;

    int i;

    for (i = 0, gs = scope_init(NULL); stdlib[i].symbol; ++i) {
	native_t *n = &stdlib[i];
	sexpr_t *l = lambda_new_native(gs, NULL, n);
	bond_t *b = bond_new(n->symbol, l);

	vector_push(gs->bonds, b);
    }

    return gs;
}

void scope_describe(object_t o) {
    if (o == NULL) {
	puts("scope was NULL");
	return;
    }

    scope_t *s = o;

    printf("is marked? [%s]\n", s->gci.ismarked ? "X" : " ");
    vector_print(s->bonds);

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
