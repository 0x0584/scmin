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
    {"list", native_list},
    {"length", native_length},
    {"cons", native_cons},
    {"car", native_car},
    {"cdr", native_cdr},
    {"set-car", native_set_car},
    {"set-cdr", native_set_cdr},

    {"and", native_and},
    {"or", native_or},
    {"not", native_not},

    {"print", native_print},

    {"+", native_add},
    {"-", native_minus},
    {"*", native_times},
    {"/", native_divid},
    {"=", native_eq},
    {"<", native_less},
    {">", native_greater},
    {"<=", native_less_eq},
    {">=", native_greater_eq},

    {"add", native_add},
    {"sub", native_minus},
    {"mul", native_times},
    {"div", native_divid},
    {"eq", native_eq},
    {"less", native_less},
    {"greater", native_greater},
    {"less-or-eq", native_less_eq},
    {"greater-or-eq", native_greater_eq},

    {"sqrt", native_sqrt},
    {"square", native_square},

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

    free(b->key);

    if (b->sexpr->gci.isglobal)
	setglobal(b->sexpr, false);

    free(b);
}

bool bond_cmp(object_t o1, object_t o2) {
    if (o1 == NULL || o2 == NULL)
	return false;

    bond_t *b1 = o1, *b2 = o2;

    return !strcmp((string_t) b1->key, (string_t) b2->key);
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

bond_t *resolve_bond(scope_t * scope, sexpr_t * expr) {
    if (!issymbol(expr))
	return NULL;

    bond_t bond = { expr->s, NULL, false };
    bond_t *resolved = NULL;
    scope_t *tscope = scope;

    while (tscope != NULL)
	if ((resolved = vector_find(tscope->bonds, &bond)))
	    break;
	else
	    tscope = tscope->parent;

    return resolved;
}

bool isbonded(scope_t * scope, sexpr_t * expr) {
    return resolve_bond(scope, expr) != NULL;
}

void bind_lambda_args(scope_t * scope, lambda_t * l, sexpr_t * args) {
    sexpr_t *foo = l->args, *bar = NULL;	/* lambda's tmp */
    sexpr_t *fuzz = args, *buzz = NULL;	/* arg's tmp */

    while (!isnil(foo)) {
	bar = car(foo), buzz = car(fuzz);
	vector_push(scope->bonds, bond_new(bar->s, buzz));
	foo = cdr(foo), fuzz = cdr(fuzz);
    }
}

scope_t *scope_init(scope_t * parent) {
    scope_t *scope = gc_alloc_scope();
    scope->parent = parent;
    return scope;
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

    scope_t *scope = o;

    printf("is marked? [%s]\n", scope->gci.ismarked ? "X" : " ");
    vector_print(scope->bonds);

    if (scope->parent) {
	puts(" parent");
	scope_describe(scope->parent);
    }
}

void setglobal(sexpr_t * sexpr, bool isglobal) {
    sexpr->gci.isglobal = isglobal;

    if (ispair(sexpr)) {
	setglobal(car(sexpr), isglobal);
	setglobal(cdr(sexpr), isglobal);
    } else if (islambda(sexpr)) {
	sexpr->l->gci.isglobal = isglobal;
	setglobal(sexpr->l->args, isglobal);
	setglobal(sexpr->l->body, isglobal);
    }
}

bool setglobalbond(scope_t * scope, bond_t * bond, bool isglobal) {
    /* this is applied only to the global scope */
    if (scope->parent == NULL) {
	setglobal(bond->sexpr, isglobal);
    }

    return scope->parent == NULL;
}

/**
 * @brief push new bonds or replace old ones
 *
 * @details here we set global scope bond members as global so
 * that they won't be cleaned up while cleaning
 *
 * @param scope the scope to push into
 * @param bond a bond to push
 */
void scope_push_bond(scope_t * scope, bond_t * bond) {
    if (scope->parent == NULL)
	setglobal(bond->sexpr, true);

    bond_t *tmp = vector_find(scope->bonds, bond);

    if (!tmp) {
	vector_push(scope->bonds, bond);
    } else {
	tmp->sexpr = bond->sexpr;
	bond_free(bond);
    }
}
