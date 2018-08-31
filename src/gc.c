/*
 * this would contain methods to handle memory using
 * mark-and-sweep garbage collection algorithm
 *
 * FIXME: figure out how to eliminate code-redundancy
 */
#include "../include/gc.h"

#include "../include/vector.h"
#include "../include/sexpr.h"
#include "../include/scope.h"

#include "../include/context.h"
#include "../include/pair.h"

static vector_t *gc_allocated_sexprs, *gc_allocated_scopes,
    *gc_allocated_contexts, *gc_allocated_lambdas;

void gc_init(void) {
    gc_allocated_sexprs = vector_new(gc_free_sexpr, sexpr_describe, NULL);
    gc_allocated_scopes = vector_new(gc_free_scope, scope_describe, NULL);
    gc_allocated_contexts = vector_new(gc_free_context,
				       context_describe, NULL);
    gc_allocated_lambdas =
	vector_new(gc_free_lambda, sexpr_describe, NULL);
}

void gc_clean(void) {
    gc_collect(true);

    vector_free(gc_allocated_sexprs);
    vector_free(gc_allocated_lambdas);
    vector_free(gc_allocated_scopes);
    vector_free(gc_allocated_contexts);
}

long gc_allocated_size(void) {
    return (gc_allocated_sexprs->size * sizeof(sexpr_t))
	+ (gc_allocated_scopes->size * sizeof(scope_t))
	+ (gc_allocated_contexts->size * sizeof(context_t))
	+ (gc_allocated_lambdas->size * sizeof(lambda_t));
}

bool_t gc_has_space_left() {
    assert(GC_FREQUENCY > 0);
    return gc_allocated_size() < GC_RATIO;
}

void gc_collect(bool_t iscleanup) {
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

    puts("\n================ sweep scopes ==================\n");
    gc_sweep_scopes(gc_allocated_scopes);

    /* puts("\n================ sweep lambdas ==================\n"); */
    /* gc_sweep_lambdas(gc_allocated_lambdas); */
    /* puts("\n++++++++++++++++++++++++++\n"); */

    /* gc_mark_stack_sexprs(gc_allocated_sexprs); */
    puts("\n================ sweep sexprs ==================\n");
    gc_sweep_sexprs(gc_allocated_sexprs);

#if GC_DEBUG == DBG_ON
    puts("================================================\n");
#endif

}

void gc_mark_stack_sexprs(vector_t * v) {
    int i;

    /* marking all that stacka as reachable */
    for (i = 0; i < v->size; ++i) {
	if (i % 2 == 0)		/* testing */
	    gc_mark_sexpr(vector_get(v, i));
    }
}

/* ==============================================================
 *		   s-expressions memory management
 * ==============================================================
 */
void gc_mark_sexpr(sexpr_t * expr) {
    assert(expr != NULL);

    /* already marked as reachable */
    if (expr->gci.ismarked)
	return;

    expr->gci.ismarked = true;

    if (isnil(expr))
	return;
    else if (ispair(expr)) {
	sexpr_t *head = car(expr), *rest = cdr(expr);

	if (head)
	    gc_mark_sexpr(head);
	if (rest)
	    gc_mark_sexpr(rest);
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

	/* lambdas are handled separately */
	if (islambda(tmp))
	    continue;

	if (!tmp->gci.ismarked) {
	    gc_free_sexpr(tmp);
	    vector_set(v, i, NULL);

#if GC_DEBUG == DBG_ON
	    ++freed;
#endif

	} else {
	    tmp->gci.ismarked = false;
	}
    }

#if GC_DEBUG == DBG_ON
    puts("sexprs stack after");
    vector_print(v);
#endif

    vector_compact(v);

#if GC_DEBUG == DBG_ON
    printf("previous: %d - current: %d - freed: %d \n",
	   size, v->size, freed);

    puts("final stack");
    vector_print(v);
#endif
}

sexpr_t *gc_alloc_sexpr(void) {

    if (!gc_has_space_left()) {
	gc_collect(true);
    }

    sexpr_t *s = malloc(sizeof *s);

    s->gci.ismarked = false;

    vector_push(gc_allocated_sexprs, s);

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

    expr = NULL;
}

/* ==============================================================
 *		      lambda memory management
 * ==============================================================
 */
lambda_t *gc_alloc_lambda(void) {
    if (!gc_has_space_left())
	gc_collect(true);

    lambda_t *l = malloc(sizeof *l);

    vector_push(gc_allocated_lambdas, l);

    return l;
}

void gc_free_lambda(object_t o) {
    if (o == NULL) return;
    lambda_t *l = o;

    gc_free_sexpr(l->args);

    if (!l->isnative) {
	gc_free_sexpr(l->body);
    }

    free(l);
}

void gc_mark_lambda(lambda_t * l) {
    if (l == NULL || l->gci.ismarked)
	return;

    l->gci.ismarked = true;

    if (!l->isnative) {
	gc_mark_sexpr(l->args);
	gc_mark_sexpr(l->body);
    }

    gc_mark_scope(l->parent);
}

void gc_sweep_lambdas(vector_t * v) {
    int i;
    lambda_t *tmp;

#if GC_DEBUG == DBG_ON
    int freed = 0, size = v->size;

    puts("labmdas stack before");
    vector_print(v);
#endif

    for (i = 0; i < v->size; ++i) {
	tmp = vector_get(v, i);

	if (!tmp->gci.ismarked) {
	    gc_free_lambda(tmp);
	    vector_set(v, i, NULL);

#if GC_DEBUG == DBG_ON
	    ++freed;
#endif

	} else {
	    tmp->gci.ismarked = false;
	}
    }

#if GC_DEBUG == DBG_ON
    puts("lambda stack after");
    vector_print(v);
#endif

    vector_compact(v);

#if GC_DEBUG == DBG_ON
    printf("previous: %d - current: %d - freed: %d \n",
	   size, v->size, freed);

    puts("final stack");
    vector_print(v);
#endif
}

/* ==============================================================
 *		       scope memory management
 * ==============================================================
 */
scope_t *gc_alloc_scope(void) {
    if (!gc_has_space_left())
	gc_collect(true);

    scope_t *s = malloc(sizeof *s);

    s->bonds = vector_new(bond_free, bond_describe, bond_cmp);
    s->parent = NULL;
    s->gci.ismarked = false;

    vector_push(gc_allocated_scopes, s);

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

void gc_mark_scope(scope_t * s) {
    if (s == NULL || s->gci.ismarked)
	return;
    int i;

    s->gci.ismarked = true;

    for (i = 0; i < s->bonds->size; ++i) {
	bond_t *b = vector_get(s->bonds, i);
	gc_mark_sexpr(b->sexpr);
    }

    gc_mark_scope(s->parent);
}

void gc_sweep_scopes(vector_t * v) {
    int i;
    scope_t *tmp;

#if GC_DEBUG == DBG_ON
    int freed = 0, size = v->size;

    puts("scope stack before");
    vector_print(v);
#endif

    for (i = 0; i < v->size; ++i) {
	tmp = vector_get(v, i);

	if (!tmp->gci.ismarked) {
	    gc_free_scope(tmp);
	    vector_set(v, i, NULL);

#if GC_DEBUG == DBG_ON
	    ++freed;
#endif

	} else {
	    tmp->gci.ismarked = false;
	}
    }

#if GC_DEBUG == DBG_ON
    puts("scope stack after");
    vector_print(v);
#endif

    vector_compact(v);

#if GC_DEBUG == DBG_ON
    printf("previous: %d - current: %d - freed: %d \n",
	   size, v->size, freed);

    puts("final stack");
    vector_print(v);
#endif
}

/* ==============================================================
 *		      context memory management
 * ==============================================================
 */
context_t *gc_alloc_context() {
    if (!gc_has_space_left())
	gc_collect(true);

    context_t *ctx = malloc(sizeof *ctx);

    vector_push(gc_allocated_contexts, ctx);

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
    sexpr_t *number = sexpr_new(T_NUMBER),
	*nil = sexpr_new(T_NIL);
    number->n = 45.2;

    sexpr_t *expr0 = cons(number, nil);

    sexpr_t *number0 = sexpr_new(T_NUMBER);
    number0->n = 44411224;

    gc_collect(true);

    puts("========== end first run ==========");
    scope_t *s = scope_init(NULL);

    sexpr_t *expr1 = sexpr_new(T_STRING);
    expr1->s = strdup("this is string");

    bond_t *b = bond_new(strdup("str"), expr1);
    vector_push(s->bonds, b);

    sexpr_t *expr2 = sexpr_new(T_SYMBOL);
    expr2->s = strdup("fuzz");

    puts("================ scope desc ============");

    scope_describe(s);

    gc_mark_scope(s);

    gc_collect(true);
}
#endif
