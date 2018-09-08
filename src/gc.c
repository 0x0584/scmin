/*
 * this would contain methods to handle memory using
 * mark-and-sweep garbage collection algorithm
 *
 * FIXME: figure out how to eliminate code-redundancy
 */
#include "../include/gc.h"

#include "../include/vector.h"
#include "../include/sexpr.h"
#include "../include/native.h"

#include "../include/scope.h"

#include "../include/context.h"
#include "../include/pair.h"

static vector_t *gc_allocd_sexprs;
static vector_t *gc_allocd_lambdas;
static vector_t *gc_allocd_scopes;
static vector_t *gc_allocd_contexts;

void gc_init(void) {
    gc_allocd_sexprs = vector_new(gc_free_sexpr, sexpr_describe, NULL);
    gc_allocd_lambdas = vector_new(gc_free_lambda, lambda_describe, NULL);
    gc_allocd_scopes = vector_new(gc_free_scope, scope_describe, NULL);
    gc_allocd_contexts = vector_new(gc_free_context,
				    context_describe, NULL);
}

void gc_clean(void) {
    vector_free(gc_allocd_sexprs);
    vector_free(gc_allocd_lambdas);
    vector_free(gc_allocd_scopes);
    vector_free(gc_allocd_contexts);
}

long gc_allocated_size(void) {
    return (gc_allocd_sexprs->size * sizeof(sexpr_t))
	+ (gc_allocd_scopes->size * sizeof(scope_t))
	+ (gc_allocd_contexts->size * sizeof(context_t))
	+ (gc_allocd_lambdas->size * sizeof(lambda_t));
}

bool gc_has_space_left() {
    assert(GC_FREQUENCY > 0);
    return gc_allocated_size() < GC_RATIO;
}

void gc_collect(bool iscleanup) {
#if GC_DEBUG == DBG_ON
    puts("================================================");
    if (iscleanup)
	printf("%s ", "final");
    else
	puts("collectng");
#endif

    /* ignore garbage collection in case of not surpassing limit */
    if (gc_has_space_left() && !iscleanup) {
	return;
    }

    #if GC_DEBUG == DBG_ON
    puts("\n================ sweep scopes ==================\n");
    #endif

    gc_sweep_scopes(gc_allocd_scopes);

    #if GC_DEBUG == DBG_ON
    puts("\n================ sweep lambdas ==================\n");
    #endif

    gc_sweep_lambdas(gc_allocd_lambdas);

    #if GC_DEBUG == DBG_ON
    puts("\n================ sweep sexprs ==================\n");
    #endif

    gc_sweep_sexprs(gc_allocd_sexprs);

#if GC_DEBUG == DBG_ON
    puts("================================================\n");
#endif

}

void gc_setmark_stack_sexprs(vector_t * v, bool mark) {
    int i;

    /* marking all that stacka as reachable */
    for (i = 0; i < v->size; ++i) {
	if (i % 2 == 0)		/* testing */
	    gc_setmark_sexpr(vector_get(v, i), mark);
    }
}

/* ==============================================================
 *		   s-expressions memory management
 * ==============================================================
 */

void gc_setmark_sexpr(sexpr_t * expr, bool mark) {
    if (expr == NULL || expr->gci.ismarked == mark)
	return;

    expr->gci.ismarked = mark;

    if (isnil(expr))
	return;
    else if (ispair(expr)) {
	gc_setmark_sexpr(car(expr), mark);
	gc_setmark_sexpr(cdr(expr), mark);
    }
}

void gc_sweep_sexprs(vector_t * v) {
    int i;
    sexpr_t *tmp;

#if GC_DEBUG == DBG_ON
    int freed = 0, size = v->size;

    puts("sexprs stack before");
    vector_print(v);
#endif

    for (i = 0; i < v->size; ++i) {
	tmp = vector_get(v, i);

	if(!tmp || (islambda(tmp) && tmp->l->isnative))
	    continue;

	if (!tmp->gci.ismarked) {
	    gc_free_sexpr(tmp);
	    vector_set(v, i, NULL);

#if GC_DEBUG == DBG_ON
	    ++freed;
#endif

	} else {
	    gc_setmark_sexpr(tmp, false);
	}
    }

    vector_compact(v);

#if GC_DEBUG == DBG_ON
    puts("final sexprs stack");
    vector_print(v);

    printf("previous: %d - current: %d - freed: %d \n",
	   size, v->size, freed);
#endif
}

sexpr_t *gc_alloc_sexpr(void) {
    if (!gc_has_space_left())
	gc_collect(true);

    sexpr_t *s = malloc(sizeof *s);

    s->gci.ismarked = false;

    vector_push(gc_allocd_sexprs, s);

    return s;
}

void gc_free_sexpr(object_t o) {
    if (o == NULL)
	return;

    sexpr_t *expr = o;

    /* because they got an allocated string */
    if (isstring(expr) || issymbol(expr)) {
	free(expr->s);
    } else if (ispair(expr)) {
	free(expr->c);
    }

    free(expr);
}

/* ==============================================================
 *		      lambda memory management
 * ==============================================================
 */

void gc_setmark_lambda(lambda_t * l, bool mark) {
    if (l == NULL || l->gci.ismarked == mark)
	return;

    l->gci.ismarked = mark;
    gc_setmark_sexpr(l->args, mark);

    if (!l->isnative) {
	gc_setmark_sexpr(l->body, mark);
    }

    gc_setmark_scope(l->parent, mark);
}

void gc_sweep_lambdas(vector_t * v) {
    int i;
    lambda_t *tmp;

#if GC_DEBUG == DBG_ON
    int freed = 0, size = v->size;

    puts("lambdas stack before");
    vector_print(v);
#endif

    for (i = 0; i < v->size; ++i) {
	tmp = vector_get(v, i);

	if (tmp->isnative)
	    continue;

	if (!tmp->gci.ismarked) {
	    gc_free_lambda(tmp);
	    vector_set(v, i, NULL);

#if GC_DEBUG == DBG_ON
	    ++freed;
#endif

	} else {
	    gc_setmark_lambda(tmp, false);
	}
    }

    vector_compact(v);

#if GC_DEBUG == DBG_ON
    puts("final lambdasstack");
    vector_print(v);

    printf("previous: %d - current: %d - freed: %d \n",
	   size, v->size, freed);
#endif
}

lambda_t *gc_alloc_lambda(void) {
    if (!gc_has_space_left())
	gc_collect(true);

    lambda_t *l = malloc(sizeof *l);

    l->gci.ismarked = false;
    l->parent = NULL;
    l->args = NULL;
    l->body = NULL;
    l->isnative = false;

    vector_push(gc_allocd_lambdas, l);

    return l;
}

void gc_free_lambda(object_t o) {
    if (o == NULL)
	return;

    lambda_t *l = o;

    free(l);
}

/* ==============================================================
 *		       scope memory management
 * ==============================================================
 */

void gc_setmark_scope(scope_t * s, bool mark) {
    if (s == NULL || s->gci.ismarked == mark)
	return;

    int i;

    s->gci.ismarked = mark;

    for (i = 0; i < s->bonds->size; ++i) {
	bond_t *b = vector_get(s->bonds, i);
	gc_setmark_sexpr(b->sexpr, mark);

	if (islambda(b->sexpr))
	    gc_setmark_lambda(b->sexpr->l, mark);
    }

    gc_setmark_scope(s->parent, mark);
}

void gc_sweep_scopes(vector_t * v) {
    scope_t *tmp;
    int i;

#if GC_DEBUG == DBG_ON
    int freed = 0, size = v->size;

    puts("scope stack before");
    vector_print(v);
#endif

    for (i = 0; i < v->size; ++i) {
	tmp = vector_get(v, i);

	if (tmp->parent == NULL) /* global scope */
	    continue;

	if (!tmp->gci.ismarked) {
	    gc_free_scope(tmp);
	    vector_set(v, i, NULL);

#if GC_DEBUG == DBG_ON
	    ++freed;
#endif

	} else {
	    gc_setmark_scope(tmp, false);
	}
    }

    vector_compact(v);

#if GC_DEBUG == DBG_ON
    printf("previous: %d - current: %d - freed: %d \n",
	   size, v->size, freed);

    puts("final stack");
    vector_print(v);
#endif
}

scope_t *gc_alloc_scope(void) {
    if (!gc_has_space_left())
	gc_collect(true);

    scope_t *s = malloc(sizeof *s);

    s->bonds = vector_new(bond_free, bond_describe, bond_cmp);
    s->parent = NULL;
    s->gci.ismarked = false;

    vector_push(gc_allocd_scopes, s);

    return s;
}

void gc_free_scope(object_t o) {
    if (o == NULL)
	return;

    scope_t *s = o;

    vector_free(s->bonds);

    if (s->parent)
	gc_free_scope(s->parent);

    free(s);
}

/* ==============================================================
 *		      context memory management
 * ==============================================================
 */

context_t *gc_alloc_context() {
    if (!gc_has_space_left())
	gc_collect(true);

    context_t *ctx = malloc(sizeof *ctx);

    vector_push(gc_allocd_contexts, ctx);

    return ctx;
}

void gc_free_context(object_t o) {
    if (o == NULL)
	return;

    context_t *c = o;

    gc_free_scope(c->scope);
    gc_free_sexpr(c->sexpr);
    gc_free_sexpr(c->childresult);

    vector_free(c->reg);
}

#if GC_DEBUG == DBG_ON
void gc_debug_memory(void) {
    /*
       puts("========== testing sexprs ==========");
       sexpr_t *number = sexpr_new(T_NUMBER), *nil = sexpr_new(T_NIL);
       number->n = 45.2;

       sexpr_t *expr0 = cons(number, nil);

       sexpr_t *number0 = sexpr_new(T_NUMBER);
       number0->n = 44411224;

       gc_collect(true);
     */
    /*
       puts("========== testing scope ==========");
       scope_t *s = scope_init(NULL);

       sexpr_t *expr1 = sexpr_new(T_STRING);
       expr1->s = strdup("this is string");

       bond_t *b = bond_new(strdup("str"), expr1);
       vector_push(s->bonds, b);

       sexpr_t *expr2 = sexpr_new(T_SYMBOL);
       expr2->s = strdup("fuzz");

       scope_describe(s);

       gc_setmark_scope(s);

       gc_collect(true);
     */
    /*
       puts("========== testing lambdas ==========");
       sexpr_t *x = sexpr_new(T_NUMBER);
       x->n = 20;

       sexpr_t *y = sexpr_new(T_NUMBER);
       y->n = 10;

       sexpr_t *args = cons(x, cons(y, sexpr_new(T_NIL)));

       native_t *n_add = malloc(sizeof *n_add);
       n_add->symbol = strdup("+");
       n_add->func = native_add;

       lambda_t *l = gc_alloc_lambda();
       l->args = args;
       l->isnative = true;
       l->native = n_add;


       sexpr_t *lam = gc_alloc_sexpr();
       lam->type = T_LAMBDA;
       lam->l = l;

       sexpr_describe(lam);

       sexpr_t *result = native_add(args);

       sexpr_describe(result);
     */
    /* gc_free_sexpr(lam); */

    /* gc_collect(true); */
}
#endif
