/**
 * @file vector.c
 *
 * @brief declarations of vector related the functions to create/update
 * and free vectors
 *
 * the main routine to handle a vector, is to first create a vector using
 * vector_new() and passing own printing and freeing functions, or `NULL`.
 * then to free the vector, vector_free() is called. to add and remove
 * members we use vector_add() or vector_push(). to get elements we either
 * use vector_get() or vector_pop(). to remove an element vector_del() or
 * to change its value we use vector_set()
 *
 * @see vector.h
 */

#include "vector.h"

const string_t separator = "\n-----------------------------------";

/**
 * @brief allocates the memory for the new vector and its members
 *
 * @param free_obj a function to free the Object.
 * @param print_obj a function to print the Object.
 * @param cmp_obj a function to compare the vector's objects
 *
 * @return a new Vector
 *
 * @note `cmp_obj` is required by vector_find()
 */
vector_t *vector_new(operation_t free_obj, operation_t print_obj,
		     compare_t cmp_obj) {
    vector_t *v = malloc(sizeof *v);

    v->capacity = 0;
    v->size = 0;

    v->objs = malloc(sizeof(object_t));

    v->free_obj = free_obj;
    v->print_obj = print_obj;
    v->cmp_obj = cmp_obj;

    /* this one is initialized using vector_set_debug() */
    v->dbg_obj = NULL;

    return v;
}

/**
 * @brief frees `o` and its objects
 *
 * if `o` has a specific free function, i.e. `free_obj()`, then a loop
 * through all the object to free each one using `free_obj()`
 *
 * @param o Vector
 *
 * @note the reason why the parameter is an object instead of a vector,
 * is to make it possible to create a vector of vectors
 */
void vector_free(object_t o) {
    if (o == NULL)
	return;

    int i;
    vector_t *v = o;

    if (v->free_obj != NULL)
	for (i = 0; i < v->capacity; ++i)
	    v->free_obj(v->objs[i]);

    if (v->objs)
	free(v->objs);
    free(v);

    o = NULL;
}

/**
 * @brief eliminating the `NULL` Objects and set `v` size and capacity
 * to the new count of the objects
 *
 * this function implements the following algorithm compacts the vector
 * while preserving its order, the idea is to keep the `NULLs` at the end
 * and bringing the objects to the front while preserving the order.
 *
 * 1. remove `NULLs` from the back while decreasing the size
 * 2. starting at the front this time
 *   + look for the first `NULL`, saved as `index_A`
 *   + look for first object, saved as `index_B`
 * 3. swap objects in `index_A` and `index_B`
 * 4. repeat start again at `index_A`
 *
 * @param v the vector to compact
 *
 * @return same vector `v` but now it's compacted
 */
vector_t *vector_compact(vector_t * v) {
    if (v == NULL)
	return NULL;
    else if (v->size == 0)
	return v;

    object_t *o = v->objs;	/* array of objects */
    int i, j, k, size;

    for (i = j = k = 0, size = v->size; i < size; ++i) {

	/* removing NULLs from the end */
	while (size && o[size - 1] == NULL)
	    size--;

	if (size == 0)
	    break;		/* vector is all NULLs */

	/* getting the first NULL index */
	for (j = i; j < size; ++j)
	    if (o[j] == NULL)
		break;

	if (j == size)
	    break;		/* reached the end */

	/* getting the first object index */
	for (k = j; k < size; ++k)
	    if (o[k] != NULL)
		break;

	if (k == size)
	    break;		/* reached the end */

	o[j] = o[k], o[k] = NULL, i = j;
    }

    assert(size >= 0);

    v->size = size;
    v->capacity = size;
    v->objs = realloc(v->objs, size * sizeof(object_t));

    return v;
}

/**
 * @brief changes the values of the `i-th` Object in `v` by the
 * new object `o`
 *
 * @param v Vector
 * @param o Object
 * @param i index where to put `o`
 */
void vector_set(vector_t * v, int i, object_t o) {
    if (v == NULL || i < 0 || i > v->size)
	return;

    v->objs[i] = o;
}

/**
 * @brief retrieve the `i-th` Object from `v` objects.
 *
 * @param v Vector
 * @param i index of the Object
 *
 * @return the `i-th` object or NULL otherwise
 *
 * @note `NULL` is returned if `(i < 0 || i > v->size)` was true,
 * i.e. out of range
 */
object_t vector_get(vector_t * v, int i) {
    return v == NULL || i < 0 || i > v->size ? NULL : v->objs[i];
}

/**
 * @brief adds `o` in the `i-th` index of `v` object's
 * array, it also increments the `v` size
 *
 * if the the we reach the maximum capacity of the `v`, then we
 * reallocate the memory adding #VECTOR_DEFAULT_CAPACITY to the
 * current capacity
 *
 * @param v Vector
 * @param o Object
 * @param i index where to put `o`
 *
 * @return the added object `o`
 *
 * @note `i` must be within the current capacity of `v`
 */
object_t vector_add(vector_t * v, object_t o, int i) {
    if (v == NULL || i < 0 || i > v->capacity)
	return NULL;

    /* if the maximum capacity is reached */
    if (v->size == v->capacity) {
	const int dc = VECTOR_DEFAULT_CAPACITY;
	const int oc = v->capacity;	/* old capacity */

	v->objs = realloc(v->objs, (oc + dc) * sizeof(object_t));
	memset(v->objs + oc, 0x00, dc * sizeof(object_t));
	v->capacity = oc + dc;
    }

    v->size++;

    return v->objs[i] = o;
}

/**
 * @brief removes `o` in the `i-th` index of `v` object's
 * array, it also decrements the `v` size
 *
 * calls `v's` `free_obj()` (if not `NULL`) to free `o` if it was not `NULL`
 *
 * @param v Vector
 * @param o Object
 * @param i index where to put `o`
 *
 * @note reallocates the objects array if necessary
 */
void vector_del(vector_t * v, int i) {
    if (v == NULL || i < 0 || i > v->size)
	return;

    if (v->free_obj != NULL)
	v->free_obj(v->objs[i]);

    v->objs[i] = NULL;
    v->size--;
}

/**
 * @brief same functionality as LIFO push
 *
 * @param v Vector
 * @param o Object
 *
 * @note calls vector_add() directly, since a LIFO push is the default
 * adding method
 */
object_t vector_push(vector_t * v, object_t o) {
    return v == NULL ? NULL : vector_add(v, o, v->size);
}

/**
 * @brief same functionally as a LIFO pop
 *
 * @param v Vector
 *
 * @return the popped object
 */
object_t vector_pop(vector_t * v) {
    if (v == NULL || v->size < 0)
	return NULL;

    int index = (v->size == 0) ? v->size : v->size - 1;
    object_t o = v->objs[index];

    v->objs[index] = NULL;
    vector_compact(v);

    return o;
}

/**
 * @brief same functionality as FIFO peek
 *
 * @param v Vector
 *
 * @return the peeked object
 */
object_t vector_peek(vector_t * v) {
    if (v == NULL || v->size == 0)
	return NULL;

    object_t o = vector_get(v, 0);

    vector_set(v, 0, NULL);
    vector_compact(v);

    return o;
}

/**
 * @brief if `o` matches any object in `v`, we return it
 *
 * using `v` `cmp_obj()`, `o` is compared with every object in the
 * objects array, if there's a match, we return it
 *
 * @param v vector of objects
 * @param o the object to match
 *
 * @return NULL if no object was found, or the object otherwise
 *
 * @note `cmp_obj()` is required, otherwise NULL would be returned
 */
object_t vector_find(vector_t * v, object_t o) {
    if (v->cmp_obj == NULL)
	return NULL;

    int i;

    for (i = 0; i < v->size; ++i) {
	if (v->cmp_obj(v->objs[i], o))
	    return v->objs[i];
    }

    return NULL;
}

/**
 * @brief prints the `v` elements using `print_obj()`
 *
 * or just the index and the address if `print_obj()` was `NULL`
 *
 * @param o Vector
 *
 * @note the reason why the argument is an object is to make it possible
 * to use it within vector of vectors
 */
void vector_print(object_t o) {
    if (o == NULL)
	return;

    int i;
    vector_t *v = o;
    for (i = 0; i < v->size; ++i) {
	if (v->objs[i] == NULL)
	    continue;

	puts(separator);

	if (v->print_obj)
	    v->print_obj(v->objs[i]);
	else
	    printf("%p -- %d\n", v->objs[i], i);

	if (i == v->size - 1)
	    puts(separator);
    }
}

/**
 * @brief sets the debugging function
 *
 * @param v vector
 * @param dbg debugging function
 *
 * @note this is the only way to set the debugging function
 */
void vector_set_debug(vector_t * v, debug_t dbg) {
    v->dbg_obj = dbg;
}

/**
 * @brief same as vector_print() but with `v's` dbg_obj() instead of
 * print_obj()
 *
 * along with the array index of each object and it's memory address. this
 * could be used also to write into a file (it must be handled outside)
 *
 * @param stream where to output
 * @param v vector to debug
 *
 * @see vector_print()
 */
void vector_debug(FILE * stream, vector_t * v) {
    int i;

    fprintf(stream, "%s\n[size:%d] [capacity:%d]\n%s\n",
	    separator, v->size, v->capacity, separator);

    for (i = 0; i < v->size; ++i) {
	fprintf(stream, "[%2.2d] - @%p", i, v->objs[i]);

	if (v->dbg_obj) {
	    fputs(" - ", stream);
	    v->dbg_obj(stream, v->objs[i]);
	}

	fputc('\n', stream);
    }

    fputs(separator, stream);
}
