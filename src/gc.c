/**
 * @file gc.c
 *
 * @brief a simple Garbage Collector implemebntation of teh mark-and-sweep
 * algorithm
 *
 * @details contains definitions to handle scopes, lambdas and s-expressions
 *
 * @todo: figure out how to eliminate code-redundancy
 */

#include "gc.h"

#include "vector.h"
#include "sexpr.h"
#include "native.h"

#include "scope.h"

#include "context.h"
#include "pair.h"

/**
 * @brief the error log
 * @see error.c
 */
extern vector_t *error_log;

/**
 * @brief a vector of allocated s-expressions in the garbage collector
 */
static vector_t *gc_allocd_sexprs;

/**
 * @brief a vector of allocated lambdas in the garbage collector
 */
static vector_t *gc_allocd_lambdas;

/**
 * @brief a vector of allocated scopes in the garbage collector
 */
static vector_t *gc_allocd_scopes;

/**
 * @brief initialize the Garbage Collector's vectors
 *
 * @see #gc_allocd_sexprs
 * @see #gc_allocd_lambdas
 * @see #gc_allocd_scopes
 */
void gc_init(void) {
    gc_allocd_sexprs = vector_new(gc_free_sexpr, sexpr_print, NULL);
    gc_allocd_lambdas = vector_new(gc_free_lambda, lambda_print, NULL);
    gc_allocd_scopes = vector_new(gc_free_scope, scope_describe, NULL);
}

/**
 * @brief free's the Garbage Collect vectors from the memory, also free's
 * the error log
 *
 * @see #gc_allocd_sexprs
 * @see #gc_allocd_lambdas
 * @see #gc_allocd_scopes
 */
void gc_clean(void) {
    gc_collect(true);

    vector_free(gc_allocd_scopes);
    vector_free(gc_allocd_lambdas);
    vector_free(gc_allocd_sexprs);

    vector_free(error_log);
}

/**
 * @brief returns the size of currently allocated memory in Bytes
 *
 * @details sum of all bytes allocated by the global vectors
 *
 * @return size of allocated memory by the GC
 *
 * @see #gc_allocd_sexprs
 * @see #gc_allocd_lambdas
 * @see #gc_allocd_scopes
 */
long gc_allocated_size(void) {
    return (gc_allocd_sexprs->size * sizeof(sexpr_t))
	+ (gc_allocd_scopes->size * sizeof(scope_t))
	+ (gc_allocd_lambdas->size * sizeof(lambda_t));
}

/**
 * @brief test whether there is some space left
 *
 * @details this is basically a test if the currently allocated space is
 * less than #GC_RATIO
 *
 * @return `true` if there is some space left
 */
bool gc_has_space_left(void) {
    assert(GC_FREQUENCY > 0);
    return gc_allocated_size() < GC_RATIO;
}

/**
 * @brief collects the objects in teh garbae collector by calling sweeping
 * functions
 *
 * @details calls the following functions, gc_sweep_sexprs(),
 * gc_sweep_lambdas() and gc_sweep_scopes()
 *
 * @param iscleanup whether to collect directly of see if there is some
 * space left
 */
void gc_collect(bool iscleanup) {
    /* ignore garbage collection in case of not surpassing limit */
    if (gc_has_space_left() && !iscleanup)
	return;

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
    puts("================================================");
    printf("%s \n", iscleanup ? "final" : "collecting");
    printf("sexprs:%d\tscopes:%d\tlambdas:%d\n",
	   gc_allocd_sexprs->size,
	   gc_allocd_scopes->size, gc_allocd_lambdas->size);
#endif

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

    puts(" -*- sexprs stack before -*- ");
    vector_print(v);
#endif

    for (i = 0; i < v->size; ++i) {
	tmp = vector_get(v, i);

	/* lambdas handled elsewhere */
	if (islambda(tmp) || tmp->gci.isglobal)
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
    puts("\n -*- final sexprs stack -*- ");
    vector_print(v);

    printf("previous:%d\tcurrent:%d\tfreed:%d\n", size, v->size, freed);
#endif
}

sexpr_t *gc_alloc_sexpr(void) {
    sexpr_t *s = malloc(sizeof *s);
    s->gci.ismarked = false;
    s->gci.isglobal = false;
    return vector_push(gc_allocd_sexprs, s);
}

void gc_free_sexpr(object_t o) {
    if (o == NULL)
	return;

    sexpr_t *expr = o;

    /* because they got an allocated string */
    if (isstring(expr) || issymbol(expr) || isnil(expr))
	free(expr->s);
    else if (ispair(expr))
	free(expr->c);

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

    if (!l->isnative)
	gc_setmark_sexpr(l->body, mark);
}

void gc_sweep_lambdas(vector_t * v) {
    int i;
    lambda_t *tmp;

#if GC_DEBUG == DBG_ON
    int freed = 0, size = v->size;

    puts(" -*- lambda stack before -*- ");
    vector_print(v);
#endif

    for (i = 0; i < v->size; ++i) {
	tmp = vector_get(v, i);

	if (tmp->isnative || tmp->gci.isglobal)
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
    puts("\n -*- final lambdas stack -*- ");
    vector_print(v);

    printf("previous: %d - current: %d - freed: %d \n",
	   size, v->size, freed);
#endif
}

lambda_t *gc_alloc_lambda(void) {
    lambda_t *l = malloc(sizeof *l);

    l->gci.ismarked = false;
    l->gci.isglobal = false;
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

    puts(" -*- scope stack before -*- ");
    vector_print(v);
#endif

    for (i = 0; i < v->size; ++i) {
	tmp = vector_get(v, i);

	if (tmp->parent == NULL)	/* global scope */
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
    puts("\n -*- final scopess stack -*- ");
    vector_print(v);

    printf("previous: %d - current: %d - freed: %d \n",
	   size, v->size, freed);

#endif
}

scope_t *gc_alloc_scope(void) {
    scope_t *s = malloc(sizeof *s);

    s->bonds = vector_new(bond_free, bond_describe, bond_cmp);
    s->parent = NULL;
    s->gci.ismarked = false;
    s->gci.isglobal = false;

    vector_push(gc_allocd_scopes, s);

    return s;
}

void gc_free_scope(object_t o) {
    if (o == NULL)
	return;

    scope_t *s = o;

    vector_free(s->bonds);

    free(s);
}

void gc_debug_memory(void) {
    vector_print(gc_allocd_scopes);
    vector_print(gc_allocd_sexprs);
    vector_print(gc_allocd_lambdas);
}
