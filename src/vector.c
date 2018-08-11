#include "../include/vector.h"

vector_t *vector_new(void) {
    vector_t *v = malloc(sizeof *v);

    v->capacity = 0;
    v->size = 0;
    v->objs = NULL;

    return v;
}

void vector_free(vector_t * v) {
    free(v->objs);
    memset(v->objs, 0, v->capacity * sizeof (object_t));
    free(v);
}

void vector_compact(vector_t * v) {
    int i, j, size = v->size;

    for (i = 0; i < size; ++i) {
	if (!(v->objs[i])) {
	    for (j = i; j < size; ++j) {
		v->objs[i] = v->objs[i + 1];
	    }
	    --size;
	}
    }

    v->size = size;
}

void vector_add(vector_t * v, object_t o) {
    if (v->size == v->capacity) {
	const int vdc = VECTOR_DEFAULT_CAPACITY;

	v->capacity += vdc;
	v->objs = realloc(v->objs, v->capacity * sizeof(object_t));
	/* memset(v->objs + vdc, 0, vdc * sizeof(object_t)); */
    }

    v->objs[v->size] = o;
    v->size++;
}

void vector_del(vector_t * v, int i) {
    if (i < 0 || i > v->size) {
	return;
    }

    free(v->objs[i]);
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

    free(v->objs[i]);
    v->objs[i] = o;
}

object_t vector_get(vector_t * v, int i) {
    if (i < 0 || i > v->size) {
	return NULL;
    }

    return v->objs[i];
}

#if defined VECTOR_DEBUG
void vector_debug(FILE * stream, vector_t * v) {
    int i;
    
    puts("--------------");
    for (i = 0; i < v->capacity; ++i) {
	fprintf(stream, "[%d] = %p\n", i, v->objs[i]);
    }
    puts("--------------");
}

void vector_testing(void) {
    vector_t *v = vector_new();
    int i, size = 30, tab[size];

    for (i = 0; i < size; ++i) {
	vector_add(v, &tab[i]);
    }

    vector_debug(stdout, v);
    vector_free(v);

    if(!v) puts("ss");
    
}
#endif
