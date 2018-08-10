#include "../include/vector.h"

vector_t *vector_new(void);
void vector_free(vector_t * v);
void vector_compact(vector_t * v);

bool_t vector_add(vector_t * v, object_t o);
bool_t vector_del(vector_t * v, int i);

bool_t vector_push(vector_t * v, object_t o);
object_t vector_pop(vector_t * v);

void vector_set(vector_t * v, int i, object_t o);
object_t vector_get(vector_t * v, int i);

#if defined VECTOR_DEBUG
void vector_debug(FILE * stream, vector_t * v);
void vector_testing(void);
#endif
