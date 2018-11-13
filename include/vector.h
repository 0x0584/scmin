#ifndef __SCMIN_VECTOR_H
#  define __SCMIN_VECTOR_H

/**
 * @file vector.h
 *
 * @brief definition of Vector Data Structure functionalities
 *
 * the vector holds an Array of objects (basically `void *` pointers)
 * which point to the desired objects. when vector_free() is called,
 * it free the objects first using the free_obj(), if not, it will
 * use the default free()
 */

#  include "main.h"

/**
 * @brief vector default capacity
 */
#  define VECTOR_DEFAULT_CAPACITY	(2<<4)

/**
 * @brief defining objects by a generic pointer
 */
typedef void *object_t;

/**
 * @brief any operation to apply on a vector member
 *
 * @param o the object
 */
typedef void (*operation_t) (object_t o);

/**
 * @brief comparison between the vector members
 *
 * @param a first object
 * @param b second object
 *
 * @return true if the objects match, false otherwise
 */
typedef bool(*compare_t) (object_t a, object_t b);

/**
 * @brief debugging an object o
 *
 * @param stream stream to debug into
 * @param o object to debug
 */
typedef void (*debug_t) (FILE * stream, object_t o);

/**
 * @brief implementation of Vector data structure using an Objects[]
 *
 * each vector has a capacity and a size, as well as own printing
 * function and free
 */
typedef struct VECTOR {
    /**
     * @brief maximum size before reallocating new space
     */
    int capacity;

    /**
     * @brief the current size, i.e. count, of the vector
     */
    int size;

    /**
     * @brief array of Objects of size capacity
     */
    object_t *objs;

    /**
     * @brief function pointer to a free-like function
     */
    operation_t free_obj;

    /**
     * @brief function pointer to a print function
     */
    operation_t print_obj;

    /**
     * @brief function pointer to a debugging function
     */
    debug_t dbg_obj;

    /**
     * @brief function pointer to a comparison function
     */
    compare_t cmp_obj;
} vector_t;

vector_t *vector_new(operation_t free_obj, operation_t print_obj,
		     compare_t cmp_obj);
void vector_free(object_t o);
vector_t *vector_compact(vector_t * v);
void vector_set(vector_t * v, int i, object_t o);
object_t vector_get(vector_t * v, int i);
object_t vector_add(vector_t * v, object_t o, int i);
void vector_del(vector_t * v, int i);
object_t vector_push(vector_t * v, object_t o);
object_t vector_pop(vector_t * v);
object_t vector_peek(vector_t * v);
object_t vector_find(vector_t * v, object_t o);
void vector_print(object_t v);
void vector_set_debug(vector_t * v, debug_t dbg);
void vector_debug(FILE * stream, vector_t * v);
void vector_testing(void);

#endif				/* __SCMIN_LIST_H */
