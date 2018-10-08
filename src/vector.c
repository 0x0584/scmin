/**
 * @file vector.c
 *
 * this file contains declaration of the functions defined in header file.
 *
 * the main routine to handle a vector, is to first create a vector using
 * vector_new() and passing own printing and freeing functions, or NULL.
 * then to free the vector, vector_free() is called. to add and remove
 * members we use vector_add() or vector_push(). to get elements we either
 * use vector_get() or vector_pop(). to remove an element vector_del() or
 * to change its value we use vector_set()
 *
 * @see @file vector.h
 */

#include "../include/vector.h"

/**
 * allocates the memory for the new vector and its members
 *
 * @param free_obj a function to free the Object.
 * @param print_obj a function to print the Object.
 *
 * @return a new Vector
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

    /* this one is initalized using vector_set_debug() */
    v->dbg_obj = NULL;

    return v;
}


/**
 * free @p v and its Object by using free_obj() to free each one
 *
 * @param v Vector
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
 * adds the Object `o` in the i-th index of `v->objs[]`
 * increments the `v->size`
 *
 * @param v Vector
 * @param o Object
 * @param i index where to put `o`
 */
object_t vector_add(vector_t * v, object_t o, int i) {
    if (i < 0 || i > v->capacity)
	return NULL;

    if (v->size == v->capacity) {
	const int dc = VECTOR_DEFAULT_CAPACITY;
	const int oc = v->capacity;	/* old capacity */

	v->objs = realloc(v->objs, (oc + dc) * sizeof(object_t));
	memset(v->objs + oc, 0, dc * sizeof(object_t));

	v->capacity = oc + dc;
    }

    v->size++;

    return v->objs[i] = o;
}

void vector_del(vector_t * v, int i) {
    if (i < 0 || i > v->size)
	return;

    v->objs[i] = NULL;
    v->size--;
}

/**
 * LIFO push
 *
 * @param v Vector
 * @param o Object
 */
object_t vector_push(vector_t * v, object_t o) {
    return vector_add(v, o, v->size);
}

/**
 * LIFO pop
 *
 * @param v Vector
 *
 * @return the popped object
 */
object_t vector_pop(vector_t * v) {
    if (v->size < 0)
	return NULL;

    int index = (v->size == 0) ? v->size : v->size - 1;
    object_t o = v->objs[index];
    v->objs[index] = NULL;

    vector_compact(v);

    return o;
}

/**
 * FIFO peek
 *
 * @param v Vector
 *
 * @return the popped object
 */
object_t vector_peek(vector_t * v) {
    assert(v != NULL);

    if (v->size == 0)
	return NULL;

    object_t o = vector_get(v, 0);
    vector_set(v, 0, NULL);

    vector_compact(v);

    return o;
}

/**
 * changes the values of the @p i the Object in @p v object array by @p o
 *
 * @param v Vector
 * @param o Object
 * @param i index where to put @p o
 */
void vector_set(vector_t * v, int i, object_t o) {
    if (i < 0 || i > v->size)
	return;
    else
	v->objs[i] = o;
}

/**
 * retrieve the i-th Object from `v->objs[]`.
 *
 * @param v Vector
 * @param i index of the Object
 *
 * @return i-th Object if (i > 0 && i < v->size),
 *	   otherwise NULL is returned
 */
object_t vector_get(vector_t * v, int i) {
    if (i < 0 || i > v->size)
	return NULL;
    else
	return v->objs[i];
}

/**
 * eliminating the NULL Objects and set the @p v
 * size and capacity to the new count of the Objects
 *
 * @param v Vector
 */
vector_t *vector_compact(vector_t * v) {
    if (v == NULL)
	return NULL;
    else if (v->size == 0)
	return v;

    /*
     * =====================================================================
     *   the idea is to keep the NULLs at the end and bringing the objects
     *        to the front while the initial order is preserved.
     * =====================================================================
     *
     * 0. remove NULLs from the back while decreasing the size
     * 1. starting at the front this time
     * 1.1. look for the first NULL, save the index_A
     * 1.2. look for first object, save the index_B
     * 2. do a swap
     * 3. repeat start again at index_A
     */

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
 * prints the @p v elements using print_obj()
 *
 * @param v Vector
 */
void vector_print(object_t o) {
    if (o == NULL)
	return;

    int i;
    vector_t *v = o;
    for (i = 0; i < v->size; ++i) {
	if (v->objs[i] == NULL)
	    continue;
	else
	    puts("-----------------------------------");
	if (v->print_obj)
	    v->print_obj(v->objs[i]);
	else
	    printf("%p -- %d\n", v->objs[i], i);
    }
    puts("-----------------------------------");
}

void vector_set_debug(vector_t * v, debug_t dbg) {
    v->dbg_obj = dbg;
}

void vector_debug(FILE * stream, vector_t * v) {
    int i;
    char *line = "--------------";

    fprintf(stream, "%s\n[size:%d] [capacity:%d]\n%s\n",
	    line, v->size, v->capacity, line);

    for (i = 0; i < v->size; ++i) {
	fprintf(stream, "[%2.2d] - @%p", i, v->objs[i]);
	if (v->dbg_obj) {
	    fputs(" - ", stream);
	    v->dbg_obj(stream, v->objs[i]);
	}
	fputc('\n', stream);
    }

    fputs(line, stream);
}

void debug_int(FILE * stream, object_t o) {
    if (!o)
	return;

    int *i = o;

    fprintf(stream, " %d ", *i);
}

void print_int(object_t o) {
    debug_int(stdout, o);
}

void free_int(object_t o) {
    if (o)
	return;
}

void vector_testing(void) {
    int i, size = 20, tab[size];
    FILE *foo = fopen("2.txt", "w"),
	*bar = fopen("3.txt", "w"), *baz = fopen("1.txt", "w");
    srand(time(NULL));

    vector_t *v = vector_new(free_int, print_int, NULL);
    vector_set_debug(v, debug_int);

    for (i = 0; i < size; ++i) {
	tab[i] = i + 1;
    }

    for (i = 0; i < size; ++i) {
	vector_push(v, &tab[i]);
    }

    puts("after pushing all objects...");
    vector_debug(baz, v);

    for (i = 0; i < v->size; ++i) {
	if (rand() % 2 == 0)
	    vector_set(v, i, NULL);
    }

    puts("after removing mod 3 elements");
    vector_debug(foo, v);

    puts("after compacting the vector");
    vector_compact(v);
    vector_debug(bar, v);

    puts("freeing the vector");
    vector_free(v);

    fclose(foo);
    fclose(bar);
    fclose(baz);
}
