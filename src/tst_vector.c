#include "../include/vector.h"

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
