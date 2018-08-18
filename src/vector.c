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

vector_t *vector_new(void (*free_func) (object_t),
		     void (*print_func) (object_t)) {
    vector_t *v = malloc(sizeof *v);

    v->capacity = 0;
    v->size = 0;
    v->objs = malloc(sizeof(object_t));

    v->print_func = print_func;
    v->free_func = free_func;

    return v;
}

void vector_free(vector_t * v) {
    int i;

    for (i = 0; i < v->capacity; ++i) {
	if (v->free_func) {
	    v->free_func(v->objs[i]);
	} else {
	    free(v->objs[i]);
	}
    }

    free(v->objs);
    free(v);
}

vector_t *vector_compact(vector_t * v) {
    int i, j, size = v->size;

    if (!v) {
	return NULL;
    }

    for (i = 0; i < size; ++i) {
	if (!(v->objs[i])) {
	    for (j = i; j < size - 1; ++j) {
		v->objs[j] = v->objs[j + 1];
	    }
	    --size;
	}
    }

    v->size = size;
    v->capacity = size;

    v->objs = realloc(v->objs, size * sizeof(object_t));

    return v;
}

void vector_add(vector_t * v, object_t o, int i) {
    /*  THAT WAS THE FUCKING BUG! GOD DAMN IT! */
    if (i < 0 || i > v->capacity) {
	return;
    }

    /* printf("%d \n", i); */

    if (v->size == v->capacity) {
	const int dc = VECTOR_DEFAULT_CAPACITY;
	const int oc = v->capacity;	/* old capacity */

	v->objs = realloc(v->objs, (oc + dc) * sizeof(object_t));
	memset(v->objs + oc, 0, dc * sizeof(object_t));

	v->capacity = oc + dc;
    }

    v->objs[i] = o;
    v->size++;
}

void vector_del(vector_t * v, int i) {
    if (i < 0 || i > v->size) {
	return;
    }

    v->objs[i] = NULL;
    v->size--;
}

void vector_push(vector_t * v, object_t o) {
    vector_add(v, o, v->size);
}

object_t vector_pop(vector_t * v) {
    if (v->size < 0) {
	return NULL;
    }

    int index = (v->size == 0) ? v->size : v->size - 1;
    object_t o = v->objs[index];
    v->objs[index] = NULL;

    vector_compact(v);

    return o;
}

void vector_set(vector_t * v, int i, object_t o) {
    if (i < 0 || i > v->size) {
	return;
    }
    v->objs[i] = o;
}

object_t vector_get(vector_t * v, int i) {
    if (i < 0 || i > v->size) {
	return NULL;
    }

    return v->objs[i];
}

void vector_print(vector_t * v) {
    int i;

#if defined VECTOR_DEBUG
    assert(v != NULL);
#endif

    if (!v) {
	return;
    }

    for (i = 0; i < v->size; ++i) {
	if (v->print_func) {
	    v->print_func(v->objs[i]);
	} else {
	    printf("%p -- %d", v->objs[i], i);
	}
    }
}

#if defined VECTOR_DEBUG
void vector_debug(FILE * stream, vector_t * v) {
    int i;

    puts("--------------");
    fprintf(stream, "[size:%d] [capacity:%d]\n", v->size, v->capacity);
    puts("--------------");
    for (i = 0; i < v->size; ++i) {
	fprintf(stream, "[%d] - (addr:%p) -", i, v->objs[i]);
	if (v->print_func) {
	    v->print_func(v->objs[i]);
	}
	puts("");
    }
    puts("--------------");
}

void vector_testing(void) {
    vector_t *v = vector_new(free, NULL);
    int i, size = 30, tab[size];


    for (i = 0; i < size; ++i) {
	tab[i] = 2 * i;
    }

    for (i = 0; i < size; ++i) {
	vector_push(v, &tab[i]);
    }

    vector_debug(stdout, v);

    for (i = 0; i < v->size; ++i) {
	fprintf(stdout, "[%d] = %d\n", i, *((int *) v->objs[i]));
    }
    puts("--------------");

    for (i = 0; i < size; ++i) {
	if (i % 3 == 0) {
	    vector_del(v, i);
	}
    }

    vector_debug(stdout, v);

    vector_compact(v);
    vector_debug(stdout, v);

    vector_free(v);
}
#endif
