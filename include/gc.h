#ifndef _SCMIN_GC_H
#  define _SCMIN_GC_H

/**
 * @file gc.h
 *
 *
 * this file contains definitions of functionalities to handle memory with
 * a minimal garbage colelctor using mark and sweep algorithm.
 *
 * the algorithm is about marking the created object to false at first, then after sweeping the memory, we clean the false-marked ones and setting the true-marked ones to true
 */
#  include "main.h"

/**
 * it's like calling free() right after each time you finished
 * but it causes some lack in performance
 */
#  define GC_ALWAYSs

struct GC_INFO {
    bool_t ismarked;		/** is marked to be cleaned */
};

/**
 * initialize the GC
 */
void gc_init(void);

/**
 * collect the garbage
 */
void gc_collect(void);

long gc_allocated_size(void);

/**
 * allocate memory for a sexpr
 */
sexpr_t *gc_alloc_sexpr(void);
void gc_free_sexpr(object_t);
void gc_mark_sexpr(sexpr_t *);
void gc_sweep_sexprs(vector_t *);

#  if GC_DEBUG == DBG_ON
/**
 * show occopied memory and useful informations
 */
void gc_debug_memory(void);
#  endif
#endif				/* _SCMIN_GC_H */
