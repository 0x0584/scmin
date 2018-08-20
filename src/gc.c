/*
 * this would contain methods to handle memory using
 * mark-and-sweep garbage collection algorithm
 */
#include "../include/gc.h"

#include "../include/vector.h"
#include "../include/sexpr.h"

/* those pointers represent all the allocated memory chunks */
static vector_t *gc_allocated_sexprs;

#if !defined GC_ALWAYS
static long gc_limit_size = (2 << 15);
#endif

void gc_init(void) {
    gc_allocated_sexprs = vector_new(gc_free_sexpr, sexpr_describe);
}

void gc_clean(void) {
    vector_free(gc_allocated_sexprs);
}

long gc_allocated_size(void) {
    return (gc_allocated_sexprs->size * sizeof(sexpr_t));
}

void gc_unmark_unused(void) {

}

void gc_collect(void) {
#if !defined GC_ALWAYS
    /* ignore garbage collection in case of not surpassing limit */
    if (gc_allocated_size() < gc_limit_size) {
	return;
    }
#endif

    gc_unmark_unused();
    gc_sweep_sexprs(gc_allocated_sexprs);

#if !defined GC_ALWAYS
    if (gc_allocated_size() > gc_limit_size) {
	gc_limit_size *= 2;	/* we double it */
    }
#endif
}

sexpr_t *gc_alloc_sexpr(void) {
    sexpr_t *s = malloc(sizeof *s);

    s->gci.ismarked = false;
    vector_push(gc_allocated_sexprs, s);

    return s;
}

void gc_free_sexpr(object_t o) {
    assert(o != NULL);

    sexpr_t *expr = o;

    /* because they got an allocated string */
    if (expr->type == T_STRING || expr->type == T_ATOM) {
	free(expr->v.s);
    }

    free(expr);
}

void gc_mark_sexpr(sexpr_t * expr) {
    assert(expr != NULL);

    if (expr != NULL) {
	return;
    }

    printf("mark:type %d \n", expr->type);

    expr->gci.ismarked = true;
}

void gc_sweep_sexprs(vector_t * v) {
    int i;
    sexpr_t *tmp;

    for (i = 0; i < v->size; ++i) {
	tmp = vector_get(v, i);

	printf("sweep: %d - %s\n", tmp->type,
	       tmp->gci.ismarked ? "marked" : "not marked");

	if (tmp) {
	    if (tmp->gci.ismarked) {
		puts("it's already marked, so set it to false\n");
		tmp->gci.ismarked = false;
	    } else {
		puts("it's not marked, so clean it up\n");
		gc_free_sexpr(tmp);
		vector_set(v, i, NULL);
	    }
	}
    }

    vector_compact(v);
}

#if GC_DEBUG == DBG_ON
void gc_debug_memory(void) {
    vector_print(gc_allocated_sexprs);
}
#endif
