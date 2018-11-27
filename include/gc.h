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

/* #  define DEBUG_GC_FULL */

/**
 *  @brief information about the garbage collection
 */
typedef struct GC_INFO {
   /**
    * @brief is marked as reachable!
    */
    bool ismarked;

    /**
     * @brief is it defined in the global scope
     */
    bool isglobal;

    bool ispinned;
} gc_info;;

void gc_init(void);
void gc_clean(void);
void gc_collect(bool iscleanup);
void gc_log(bool iscleanup);
long gc_allocated_size(void);

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

void *gc_malloc(size_t size);
void *gc_realloc(void *ptr, size_t size);

void gc_setpin_sexpr(sexpr_t * expr, bool pin);
void gc_setpin_scope(scope_t * scope, bool pin);
void gc_setpin_lambda(lambda_t * expr, bool pin);

void gc_debug_memory(void);
#endif				/* _SCMIN_GC_H */
