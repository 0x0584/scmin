/**
 * @file gc.c
 *
 * @brief a simple Garbage Collector implementation of the mark-and-sweep
 * algorithm
 *
 * @details add more details later
 *
 * @todo figure out how to eliminate code-redundancy
 *
 * @todo find where do pinned sexprs go while recursion
 */

#include "gc.h"

#include "vector.h"
#include "sexpr.h"
#include "native.h"

#include "scope.h"
#include "eval.h"
#include "pair.h"

/**
 * @brief limiting the stack size after each cleaning up so that we can
 * call the GC less often
 */
static long gc_stack_limit = 0x0584;

/**
 * @brief the error log
 * @see error.c
 */
extern vector_t *error_log;

extern vector_t *eval_stack;

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
    vector_free(eval_stack);
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
    printf("%-8s %8d * %-8s %8d * %-8s %8d\n",
	   "before:", a, "after:", b, "diff:", b - a);
}

void gc_log(bool iscleanup) {
    printf("==%s====================================================\n",
	   !iscleanup ? " # " : "===");
    printf("%-8s %8d * %-8s %8d * %-8s %8d\n",
	   "scopes:", gc_allocd_scopes->size,
	   "lambdas:", gc_allocd_lambdas->size,
	   "sexprs:", gc_allocd_sexprs->size);
    printf
	("%15ld B * %15ld B * %15ld B\n=========================================================\nstack size: %13ld B * limit size: %13ld B\n",
	 gc_allocated_scopes_size(),
	 gc_allocd_lambdas->size * sizeof(lambda_t),
	 gc_allocd_sexprs->size * sizeof(sexpr_t), gc_allocated_size(),
	 gc_stack_limit);
    puts("=========================================================");
}

/**
 * @brief test whether there is some space left
 *
 * @details this is basically a test if the currently allocated space is
 * less than gc_stack_limit
 *
 * @return `true` if there is some space left
 */
bool gc_has_space_left(void) {
    return gc_allocated_size() < gc_stack_limit;
}

void gc_pin_eval_stack(void) {
    if (eval_stack == NULL)
	return;

    int i, j;
    context_t *tmp = NULL;
    sexpr_t **stmp = NULL;

    for (i = 0; i < eval_stack->size; ++i) {
	tmp = vector_get(eval_stack, i);

	gc_setpin_scope(tmp->scope, true);
	gc_setpin_sexpr(tmp->result, true);
	gc_setpin_sexpr(tmp->sexpr, true);

	for (j = 0; j < tmp->locals->size; ++j) {
	    stmp = vector_get(tmp->locals, j);
	    gc_setpin_sexpr(*stmp, true);
	}
    }
}

void gc_setpin_scope(scope_t * scope, bool pin) {
    if (scope == NULL || scope->gci.ispinned == pin)
	return;

    int i;

    scope->gci.ispinned = pin;

    for (i = 0; i < scope->bonds->size; ++i) {
	bond_t *b = vector_get(scope->bonds, i);
	gc_setpin_sexpr(b->sexpr, pin);
    }

    gc_setpin_scope(scope->parent, pin);
}

void gc_setpin_lambda(lambda_t * expr, bool pin) {
    if (expr == NULL || expr->gci.ispinned == pin)
	return;

    expr->gci.ispinned = pin;

    gc_setpin_sexpr(expr->args, pin);

    if (!expr->isnative)
	gc_setpin_sexpr(expr->body, pin);
}

void gc_setpin_sexpr(sexpr_t * expr, bool pin) {
    if (expr == NULL)
	return;

    expr->gci.ispinned = pin;

    if (ispair(expr)) {
	gc_setpin_sexpr(car(expr), pin);
	gc_setpin_sexpr(cdr(expr), pin);
    } else if (islambda(expr)) {
	gc_setpin_lambda(expr->l, pin);
    }
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

    gc_setmark_scope(get_global_scope(), true);
    gc_pin_eval_stack();

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
	gc_stack_limit = gc_allocated_size();

#if DEBUG_GC == DEBUG_ON
    gc_log(iscleanup);
#endif
}

void *gc_malloc(size_t size) {
    void *foo = malloc(size);

    if (foo == NULL) {
	fprintf(stderr, "ERROR! cannot allocate %ld", size);
	exit(-1);
    }

    memset(foo, 0x00, size);

    return foo;
}

void *gc_realloc(void *ptr, size_t size) {
    void *foo = realloc(ptr, size);

    if (foo == NULL) {
	fprintf(stderr, "ERROR! cannot reallocate %ld", size);
	exit(-1);
    }

    return foo;
}

/* ==============================================================
 *		   s-expressions memory management
 * ==============================================================
 */

void gc_setmark_sexpr(sexpr_t * expr, bool mark) {
    if (expr == NULL)
	return;

    expr->gci.ismarked = mark;

    if (ispair(expr)) {
	gc_setmark_sexpr(car(expr), mark);
	gc_setmark_sexpr(cdr(expr), mark);
    } else if (islambda(expr)) {
	gc_setmark_lambda(expr->l, mark);
    }
}

void gc_sweep_sexprs(vector_t * v) {
    int i;
    sexpr_t *tmp;

#if DEBUG_GC == DEBUG_ON
    int size = v->size;

#  ifdef DEBUG_GC_FULL
    puts(" -*- sexprs stack before -*- ");
    vector_print(v);
    puts("==============================");
#  endif
#endif

    for (i = 0; i < v->size; ++i) {
	if ((tmp = vector_get(v, i)) == NULL)
	    continue;

	/* lambdas handled elsewhere */
	if (islambda(tmp) || tmp->gci.isglobal || tmp->gci.ispinned)
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
#  ifdef DEBUG_GC_FULL
    puts("\n -*- final sexprs stack -*- ");
    vector_print(v);
    puts("==============================");
#  endif
    gc_sweep_log(size, v->size);
#endif
}

sexpr_t *gc_alloc_sexpr(void) {
    sexpr_t *s = gc_malloc(sizeof *s);
    s->gci.ismarked = false;
    s->gci.isglobal = false;
    s->gci.ispinned = false;
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

#  ifdef DEBUG_GC_FULL
    puts(" -*- lambda stack before -*- ");
    vector_print(v);
#  endif
#endif

    for (i = 0; i < v->size; ++i) {
	if ((tmp = vector_get(v, i)) == NULL)
	    continue;

	if (tmp->isnative || tmp->gci.isglobal || tmp->gci.ispinned)
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
#  ifdef DEBUG_GC_FULL
    puts("\n -*- final lambdas stack -*- ");
    vector_print(v);
#  endif

    gc_sweep_log(size, v->size);
#endif
}

lambda_t *gc_alloc_lambda(void) {
    lambda_t *l = gc_malloc(sizeof *l);

    l->gci.ismarked = false;
    l->gci.isglobal = false;
    l->gci.ispinned = false;
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

    /* args and the body are both s-expressions both are
     * handled while freeing sexprs */
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
    }

    gc_setmark_scope(s->parent, mark);
}

void gc_sweep_scopes(vector_t * v) {
    scope_t *tmp;
    int i;

#if DEBUG_GC == DEBUG_ON
    int size = v->size;
#  ifdef DEBUG_GC_FULL
    puts(" -*- scope stack before -*- ");
    vector_print(v);
    puts("==============================");
#  endif
#endif

    for (i = 0; i < v->size; ++i) {
	if ((tmp = vector_get(v, i)) == NULL)
	    continue;

	if (tmp->parent == NULL || tmp->gci.ispinned)	/* global scope */
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
#  ifdef DEBUG_GC_FULL
    puts("\n -*- final scopes stack -*- ");
    vector_print(v);
    puts("==============================");
#  endif
    gc_sweep_log(size, v->size);
#endif
}

scope_t *gc_alloc_scope(void) {
    scope_t *s = gc_malloc(sizeof *s);

    s->bonds = vector_new(bond_free, bond_describe, bond_cmp);
    s->parent = NULL;
    s->gci.ismarked = false;
    s->gci.isglobal = false;
    s->gci.ispinned = false;
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
