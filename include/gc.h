#ifndef _SCMIN_GC_H
#  define _SCMIN_GC_H

/**
 * @file gc.h
 *
 *
 * this file contains definitions of functionalities to handle memory
 * with a minimal garbage colelctor using mark and sweep algorithm.
 *
 * the algorithm is about marking the created object to false at
 * first, then after sweeping the memory, we clean the false-marked
 * ones and setting the true-marked ones to true
 */

#  include "main.h"

#  define GC_STACK_LIMIT_SIZE	(2 << 15)

#  define GC_FREQUENCY		 4

#  define GC_RATIO		(GC_STACK_LIMIT_SIZE / GC_FREQUENCY)

struct GC_INFO {
    bool ismarked;		/** is marked as reachable! */
};

/**
 * initialize the GC
 */
void gc_init(void);

void gc_clean(void);

/**
 * collect the garbage
 */
void gc_collect(bool final);

long gc_allocated_size(void);

/**
 * allocate memory for a sexpr
 */
sexpr_t *gc_alloc_sexpr(void);
void gc_free_sexpr(object_t o);
void gc_setmark_sexpr(sexpr_t * expr, bool mark);
void gc_sweep_sexprs(vector_t * v);

lambda_t *gc_alloc_lambda(void);
void gc_free_lambda(object_t o);
void gc_setmark_lambda(lambda_t * lambda, bool mark);
void gc_sweep_lambdas(vector_t * v);

scope_t *gc_alloc_scope(void);
void gc_free_scope(object_t o);
void gc_setmark_scope(scope_t * scope, bool mark);
void gc_sweep_scopes(vector_t * v);

context_t *gc_alloc_context();
void gc_free_context(object_t o);
void gc_setmark_context(context_t * scope, bool mark);
void gc_sweep_context(vector_t * v);

void gc_setmark_stack_sexprs(vector_t * v, bool mark);

#  if GC_DEBUG == DBG_ON
/**
 * show occopied memory and useful informations
 */
void gc_debug_memory(void);
#  endif
#endif				/* _SCMIN_GC_H */
