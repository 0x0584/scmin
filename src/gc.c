/*
 * this would contain methods to handle memory using
 * mark-and-sweep garbage collection algorithm
 */
#include "../include/gc.h"

void gc_init(void);
void gc_collect(void);

value_t *gc_alloc_value(void);
void gc_mark_value(value_t *);
void gc_free_value(value_t *);

lambda_t *gc_alloc_lambda(void);
void gc_mark_lambda(lambda_t *);
void gc_free_lambda(lambda_t *);

scope_t *gc_alloc_scope(void);
void gc_mark_scope(scope_t *);
void gc_free_scope(scope_t *);

#if defined GC_DEBUG
void gc_debug_memory(FILE * stream) {
    fprintf(stream, "%s", "debug");
}
#endif
