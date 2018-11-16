/**
 * @file gc.c
 *
 * @brief a simple Garbage Collector implementation of the mark-and-sweep
 * algorithm
 *
 * @details contains definitions to handle scopes, lambdas and s-expressions
 *
 * @todo figure out how to eliminate code-redundancy
 */

#include "gc.h"

#include "vector.h"
#include "sexpr.h"
#include "native.h"

#include "scope.h"
#include "pair.h"

static long gc_stack_limit = 0x0584;

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
 */
void gc_init(void) {
    gc_allocd_sexprs = vector_new(gc_free_sexpr, sexpr_print, NULL);
    gc_allocd_lambdas = vector_new(gc_free_lambda, lambda_print, NULL);
    gc_allocd_scopes = vector_new(gc_free_scope, scope_describe, NULL);
}

/**
 * @brief free's the Garbage Collect vectors from the memory, also free's
 * the error log
 */
void gc_clean(void) {
    vector_free(gc_allocd_scopes);
    vector_free(gc_allocd_lambdas);
    vector_free(gc_allocd_sexprs);

    vector_free(error_log);
}

long gc_allocated_scopes_size() {
    long scopes_size = (gc_allocd_scopes->size * sizeof(scope_t));
    int i;

    for (i = 0; i < gc_allocd_scopes->size; ++i) {
	scope_t *s = vector_get(gc_allocd_scopes, i);
	scopes_size += (s->bonds->size * sizeof(bond_t));
    }

    return scopes_size;
}

/**
 * @brief returns the size of currently allocated memory in Bytes
 *
 * @details sum of all bytes allocated by the global vectors
 *
 * @return size of allocated memory by the GC
 */
long gc_allocated_size(void) {

    return (gc_allocd_sexprs->size * sizeof(sexpr_t))
	+ gc_allocated_scopes_size()
	+ (gc_allocd_lambdas->size * sizeof(lambda_t));
}

void gc_sweep_log(int a, int b) {
printf("%-8s %8d - %-8s %8d - %-8s %8d\n",
	   "before:", a, "after:", b, "diff:", b - a);
}

void gc_log(bool iscleanup) {
    printf("==%s====================================================\n",
	   !iscleanup ? " # " : "===");
    printf("%-8s %8d - %-8s %8d - %-8s %8d\n",
	   "scopes:", gc_allocd_scopes->size,
	   "lambdas:", gc_allocd_lambdas->size,
	   "sexprs:",  gc_allocd_sexprs->size);
    printf("%15ld B - %15ld B - %15ld B\nstack size: %13ld B - limit size: %13ld B\n",
	   gc_allocated_scopes_size(),
	   gc_allocd_lambdas->size * sizeof(lambda_t),
	   gc_allocd_sexprs->size * sizeof(sexpr_t),
	   gc_allocated_size(),
	   gc_stack_limit);
    puts("=========================================================");
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
    return gc_allocated_size() < gc_stack_limit;
}

/**
 * @brief collects the objects in the garbage collector by calling sweeping
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

#if DEBUG_GC == DEBUG_ON
    gc_log(iscleanup);
#endif

#if DEBUG_GC == DEBUG_ON
    puts("===================== sweep scopes ======================");
#endif

    gc_sweep_scopes(gc_allocd_scopes);

#if DEBUG_GC == DEBUG_ON
    puts("===================== sweep lambdas =====================");
#endif

    gc_sweep_lambdas(gc_allocd_lambdas);

#if DEBUG_GC == DEBUG_ON
    puts("===================== sweep sexprs ======================");
#endif

    gc_sweep_sexprs(gc_allocd_sexprs);


    if (!gc_has_space_left())
	gc_stack_limit += gc_allocated_size();

#if DEBUG_GC == DEBUG_ON
    /* gc_log(iscleanup); */
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

#if DEBUG_GC == DEBUG_ON
    int size = v->size;

    if (DEBUG_FULL) {
	puts(" -*- sexprs stack before -*- ");
	vector_print(v);
    }
#endif

    for (i = 0; i < v->size; ++i) {
	tmp = vector_get(v, i);

	/* lambdas handled elsewhere */
	if (islambda(tmp) || tmp->gci.isglobal)
	    continue;

	if (!tmp->gci.ismarked) {
	    gc_free_sexpr(tmp);
	    vector_set(v, i, NULL);

	} else {
	    gc_setmark_sexpr(tmp, false);
	}
    }

    vector_compact(v);

#if DEBUG_GC == DEBUG_ON
    if (DEBUG_FULL) {
	puts("\n -*- final sexprs stack -*- ");
	vector_print(v);
    }

    gc_sweep_log(size, v->size);
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

#if DEBUG_GC == DEBUG_ON
    int size = v->size;

    if (DEBUG_FULL) {
	puts(" -*- lambda stack before -*- ");
	vector_print(v);
    }
#endif

    for (i = 0; i < v->size; ++i) {
	tmp = vector_get(v, i);

	if (tmp->isnative || tmp->gci.isglobal)
	    continue;

	if (!tmp->gci.ismarked) {
	    gc_free_lambda(tmp);
	    vector_set(v, i, NULL);

	} else {
	    gc_setmark_lambda(tmp, false);
	}
    }

    vector_compact(v);

#if DEBUG_GC == DEBUG_ON
    if (DEBUG_FULL) {
	puts("\n -*- final lambdas stack -*- ");
	vector_print(v);
    }

    gc_sweep_log(size, v->size);
#endif
}

lambda_t *gc_alloc_lambda(void) {
    lambda_t *l = malloc(sizeof *l);

    l->gci.ismarked = false;
    l->gci.isglobal = false;
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

#if DEBUG_GC == DEBUG_ON
    int size = v->size;

    if (DEBUG_FULL) {
	puts(" -*- scope stack before -*- ");
	vector_print(v);
    }
#endif

    for (i = 0; i < v->size; ++i) {
	tmp = vector_get(v, i);

	if (tmp->parent == NULL)	/* global scope */
	    continue;

	if (!tmp->gci.ismarked) {
	    gc_free_scope(tmp);
	    vector_set(v, i, NULL);

	} else {
	    gc_setmark_scope(tmp, false);
	}
    }

    vector_compact(v);

#if DEBUG_GC == DEBUG_ON
    if (DEBUG_FULL) {
	puts("\n -*- final scopes stack -*- ");
	vector_print(v);
    }

    gc_sweep_log(size, v->size);
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
