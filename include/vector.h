#ifndef __SCMIN_VECTOR_H
#  define __SCMIN_VECTOR_H
#  include "main.h"
#  define VECTOR_DEBUG
#  define VECTOR_DEFAULT_CAPACITY	(2<<4)

struct VECTOR {
    int capacity;		/*! maximum size before reallocating */
    int size;			/*! current size */
    object_t *objs;		/*! array of objects */
};

/* this is handled manually */
vector_t *vector_new(void);
void vector_free(vector_t * v);

/*! reduce the size of the array to an optimal size  */
void vector_compact(vector_t * v);

/*! add the object to the head of the array */
void vector_add(vector_t * v, object_t o);
void vector_del(vector_t * v, int i);

/*! pushes the object to the head of the array */
void vector_push(vector_t * v, object_t o);
/*! get the object on the head */
object_t vector_pop(vector_t * v);

void vector_set(vector_t * v, int i, object_t o);
object_t vector_get(vector_t * v, int i);

void vector_log(vector_t * v, void (*logger)(object_t));

#  if defined VECTOR_DEBUG
void vector_debug(FILE * stream, vector_t * v);
void vector_testing(void);
#  endif

#endif				/* __SCMIN_LIST_H */
