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

    return v;
}


/**
 * free @p v and its Object by using free_obj() to free each one
 *
 * @param v Vector
 */
void vector_free(vector_t * v) {
    int i;

    if (v->free_obj != NULL) {
	for (i = 0; i < v->capacity; ++i) {
	    v->free_obj(v->objs[i]);
	}
    }

    free(v->objs);
    free(v);
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

    return (v->objs[i] = o);
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
 * size and capacity to the new counting of the Objects
 *
 * @param v Vector
 */
vector_t *vector_compact(vector_t * v) {
    if (v == NULL)
	return NULL;
    else if (v->size == 0)
	return v;

    int i = 0, j, size = v->size;

    /* this would literaly shift elements in the array
     * and there'is a case where the first element
     * maybe be NULL.
     *
     * in that scenario, we would leave a pointer allocated
     * and set the size and capacity to 0 */
    for (i = 0; i < size; ++i) {
	if (!(v->objs[i])) {
	    for (j = i; j < size - 1; ++j) {
		v->objs[j] = v->objs[j + 1];
		if (!v->objs[j + 1])
		    --size;
	    }
	    --size;
	}
    }

    /* in case the vecy first object was NULL
     * but not on the first run */
    if (v->objs[0] == NULL && i != 0 && size > 0) {
	size = 0;
    }

    assert(size >= 0);		/* not sure! */

    v->size = size;
    v->capacity = size;

    if (size == 0) {
	++size;			/* keep the pointer alive */
    }

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
void vector_print(vector_t * v) {
    if (v == NULL)
	return;

    int i;

    for (i = 0; i < v->size; ++i) {
	if (v->objs[i] == NULL)
	    continue;

	puts("//////////////////////////////");
	if (v->print_obj)
	    v->print_obj(v->objs[i]);
	else
	    printf("%p -- %d", v->objs[i], i);

    }
    puts("//////////////////////////////");

}

void vector_debug(FILE * stream, vector_t * v) {
    int i;

    puts("--------------");
    fprintf(stream, "[size:%d] [capacity:%d]\n", v->size, v->capacity);
    puts("--------------");
    for (i = 0; i < v->size; ++i) {
	fprintf(stream, "[%d] - (addr:%p) -", i, v->objs[i]);
	if (v->print_obj) {
	    v->print_obj(v->objs[i]);
	}
	puts("");
    }
    puts("--------------");
}

void print_int(object_t o) {
    if (!o)
	return;
    int *i = o;

    printf(" %d \n", *i);
}

void free_int(object_t o) {
    if (o)
    return;
}

void vector_testing(void) {
    vector_t *v = vector_new(free_int, print_int, NULL);
    int i, size = 15, tab[size];

    for (i = 0; i < size; ++i) {
	tab[i] = i + 1;
    }

    for (i = 0; i < size; ++i) {
	vector_push(v, &tab[i]);
    }

    puts("after pushing all objects...");
    vector_debug(stdout, v);

    for (i = 0; i < v->size; ++i) {
	if (i % 3 == 0) {
	    vector_set(v, i, NULL);
	}
    }

    puts("after removing mod 3 elements");
    vector_debug(stdout, v);

    puts("after compacting the vector");
    vector_compact(v);
    vector_debug(stdout, v);

    puts("freeing the vector");
    vector_free(v);
}
