#include "../include/vector.h"

vector_t *vector_new(void) {
    vector_t *v = malloc(sizeof *v);

    v->capacity = 0;
    v->size = 0;
    v->objs = malloc(sizeof(object_t));

    return v;
}

void vector_free(vector_t * v) {
    free(v->objs);
    free(v);
}

void vector_compact(vector_t * v) {
    int i, j, size = v->size;

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
}

void vector_add(vector_t * v, object_t o) {
    if (v->size == v->capacity) {
	const int dc = VECTOR_DEFAULT_CAPACITY;
	const int oc = v->capacity;	/* old capacity */

	v->objs = realloc(v->objs, (oc + dc) * sizeof(object_t));
	memset(v->objs + oc, 0, dc * sizeof(object_t));

	v->capacity = oc + dc;
    }

    v->objs[v->size] = o;
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
    vector_add(v, o);
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

void vector_log(vector_t * v, void (*logger)(object_t)) {
    int i;
    
    for (i = 0; i < v->size; ++i) {
	logger(v->objs[i]);
    }
}

#if defined VECTOR_DEBUG
void vector_debug(FILE * stream, vector_t * v) {
    int i;

    puts("--------------");
    fprintf(stream, "[size:%d] [capacity:%d]\n", v->size, v->capacity);
    puts("--------------");
    for (i = 0; i < v->capacity; ++i) {
	fprintf(stream, "[%d] - %p \n", i , v->objs[i]);
    }
    puts("--------------");
}

void vector_testing(void) {
    vector_t *v = vector_new();
    int i, size = 30, tab[size];


    for (i = 0; i < size; ++i) {
	tab[i] = 2 * i;
    }

    for (i = 0; i < size; ++i) {
	vector_add(v, &tab[i]);
    }

    vector_debug(stdout, v);
    
    for (i = 0; i < v->size; ++i) {
	fprintf(stdout, "[%d] = %d\n", i, *((int *)v->objs[i]));
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