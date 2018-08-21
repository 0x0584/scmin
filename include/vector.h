#ifndef __SCMIN_VECTOR_H
#  define __SCMIN_VECTOR_H

/**
 * @file vector.h
 *
 * this file contains definitions of Vector Data Structure functionalities,
 * like creating a new vector, free a vector from the memory, and member
 * manipulations.
 *
 * the vector holds an Array of objects (basically void * pointers) which point
 * to the desired objects. when vector_free() is called, it free the objects
 * first using the free_func(), if not, it will use the default free()
 */

#  include "main.h"

/**
 * vector default capacity
 */
#  define VECTOR_DEFAULT_CAPACITY	(2<<4)

/**
 * doubling the capacity instead of adding it up each time
 * example: current *= 2 instead of current += DEFAULT
 */
#  define VECTOR_DOUBLE_CAPACITY 0

/**
 * @brief implementation of Vector data structure using an Objects[]
 *
 * each vector has a capacity and a size, as well as own printing
 * function and free function
 *
 * @note the memory management is done manually for the moment
 */
struct VECTOR {
    int capacity;	       /** maximum size before reallocating */
    int size;		       /** current size */
    object_t *objs;	       /** array of Objects */
    void (*free_func) (object_t);/** to free the Object */
    void (*print_func) (object_t);/** to print the Object  */
};

/**
 * allocates the memory for the new vector and its members
 *
 * @param free_func a function the free the Object.
 *	  if NULL, stdlib/free is used
 * @param print_func a function to print the Object.
 *	  if NULL, stdlib/printf is used to print the pointer
 *
 * @return a new Vector
 */
vector_t *vector_new(void (*print_func) (object_t),
		     void (*free_func) (object_t));

/**
 * free @p v and its Object by using free_func() to free each one
 *
 * @param v Vector
 */
void vector_free(vector_t * v);

/**
 * prints the @p v elements using print_func()
 *
 * @param v Vector
 */
void vector_print(vector_t * v);

/**
 * eliminating the NULL Objects and set the @p v
 * size and capacity to the new counting of the Objects
 *
 * @param v Vector
 */
vector_t *vector_compact(vector_t * v);

/**
 * changes the values of the @p i the Object in @p v object array by @p o
 *
 * @param v Vector
 * @param o Object
 * @param i index where to put `o`
 */
void vector_set(vector_t * v, int i, object_t o);


/**
 * retrieve the i-th Object from `v->objs[]`.
 *
 * @param v Vector
 * @param i index of the Object
 *
 * @return i-th Object if (i > 0 && i < v->size),
 *	   otherwise NULL is returned

 */
object_t vector_get(vector_t * v, int i);

/**
 * adds the Object `o` in the i-th index of `v->objs[]`
 * increments the `v->size`
 *
 * @param v Vector
 * @param o Object
 * @param i index where to put `o`
 */
void vector_add(vector_t * v, object_t o, int i);

/**
 * removes the Object `o` of the @p i th index from the @p v Object[]
 * free_func() is called to `free()` Object
 *
 * @param v Vector
 * @param i index of the Object
 */
void vector_del(vector_t * v, int i);

/**
 * pushes the Object as a stack push.
 * same as calling add() with index `(v->size - 1)` if v->size > 0
 * otherwise use 0 as index
 *
 * @param v Vector
 * @param o Object
 */
void vector_push(vector_t * v, object_t o);

/**
 * get the Object as a stack pop -- calling get() with index 0
 *
 * @param v Vector
 */
object_t vector_pop(vector_t * v);

object_t vector_peek(vector_t *v);

#  if VECTOR_DEBUG == DBG_ON
/* debugging functionalities */
void vector_debug(FILE * stream, vector_t * v);
void vector_testing(void);
#  endif

#endif				/* __SCMIN_LIST_H */
