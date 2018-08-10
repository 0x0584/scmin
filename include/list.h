#ifndef __SCMIN_LIST_H
#  define __SCMIN_LIST_H
#  include "main.h"

typedef struct NODE node_t;
typedef struct LIST list_t;

struct NODE {
    struct NODE *next, *prev;
    void *value;
};

struct LIST {
    node_t *head, *tail;
};

node_t *node_init(void *value, node_t * prev, node_t * next);
void node_free(node_t * node);
list_t *list_init(void);
bool_t list_isempty(list_t * list);
void list_free(list_t * list);
size_t list_length(list_t * list);
void list_log(FILE * stream, list_t * list);
node_t *list_push(node_t * node, void *value);
void *list_pop(node_t * node);
node_t *list_find(list_t * list, void *value, bool_t ishead);
list_t *list_random(size_t limit);
bool_t list_isin(void *value, list_t * list);
list_t *list_distinct(list_t * list);
void testing_linked_lists(FILE * stream);

#endif				/* __SCMIN_LIST_H */
