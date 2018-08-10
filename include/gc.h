#ifndef _SCMIN_GC_H
#  define _SCMIN_GC_H
#  include "main.h"

/*! it's like calling free() right after each time you finished
 * but it causes some lack in performance */
#  define GC_ALWAYS

/*! show helpful debuging messages */
#  define GC_DEBUG

struct GC_INFO {
    int id;			/*! object id */
    int marked;			/*! is marked to be cleaned */
};

/*! initialize the GC  */
void gc_init(void);

/*! collect the garbage */
void gc_collect(void);

#  if defined GC_DEBUG
/*! show occopied memory and useful informations */
void gc_debug_memory(FILE * stream);
#  endif

/*! allocate memory for a value	 */
value_t *gc_alloc_value(void);
void gc_mark_value(value_t *);
void gc_free_value(value_t *);

lambda_t *gc_alloc_lambda(void);
void gc_mark_lambda(lambda_t *);
void gc_free_lambda(lambda_t *);

scope_t *gc_alloc_scope(void);
void gc_mark_scope(scope_t *);
void gc_free_scope(scope_t *);

#endif				/* _SCMIN_GC_H */
