#include "../include/list.h"

node_t *node_init(void *value, node_t * prev, node_t * next) {
    node_t *new = malloc(sizeof *new);

    new->value = value;
    new->prev = prev;
    new->next = next;

    return new;
}

void node_free(node_t * node) {
    free(node);
}

list_t *list_init(void) {
    list_t *foo;
    node_t *head = node_init(NULL, NULL, NULL);
    node_t *tail = node_init(NULL, NULL, NULL);

    head->next = tail;
    tail->prev = head;
    
    foo = malloc(sizeof *foo);
    foo->head = head;
    foo->tail = tail;
    
    return foo;
}

bool_t list_isempty(list_t * list) {
    return list->head->next == list->tail;
}

void list_free(list_t * list) {
    node_t *tmp, *head = list->head;

    /* another approach would be using pop() but this one
     * is more efficiant since it's just permutation of node */
    while (!list_isempty(list)) {
	tmp = head->next;
	free(tmp->value);
	head->next = tmp->next;
	free(tmp);
    }

    free(list->head);
    free(list->tail);
    free(list);
}

size_t list_length(list_t * list) {
    node_t *tmp = list->head->next;
    size_t count = 0;

    while (tmp->next) {
	++count;
	tmp = tmp->next;
    }

    return count;
}

void list_log(FILE * stream, list_t * list) {
    if (list_isempty(list))
	return;

    node_t *tmp = list->head->next;
    size_t count = 0;

    fputs("\n[ tail <-\n\t", stream);

    while (tmp->next) {
	fprintf(stream, "%p", tmp->value);
	tmp = tmp->next;
	if (tmp->next) {
	    fprintf(stream, ",%s", count++ % 10 == 9 ? "\n\t" : " ");
	}
    }

    fputs("\n <- head ]\n", stream);
}

node_t *list_push(node_t * node, void *value) {
    node_t *tmp, *new;

    /* this is the tail */
    if (!node->next)
	return list_push(node->prev, value);

    new = node_init(value, node, node->next);

    tmp = node->next;
    node->next = new;
    tmp->prev = new;

    return new;
}

void *list_pop(node_t * node) {
    node_t *tmp;
    void *data;

    if (!(tmp = node->next))
	return NULL;

    data = tmp->value;
    node->next = tmp->next;
    tmp->next->prev = node;

    node_free(tmp);

    return data;
}

node_t *list_find(list_t * list, void *value, bool_t ishead) {
    node_t *tmp = NULL, *target = NULL;
    tmp = ishead ? list->head->next : list->tail->prev;

    while (tmp) {
	if (tmp->value == value) {
	    target = tmp;
	    break;
	} else {
	    tmp = ishead ? tmp->next : tmp->prev;
	}
    }

    /* for-version */
    /*
       for (; tmp; tmp = ishead ? tmp->next : tmp->prev) {
       if (tmp->value == value) {
       return tmp;
       }
       }
     */
    return target;
}

list_t *list_random(size_t limit) {
    /* list_t *tmp = list_init(); */

    /* size_t rand_seed = rand() % 0xFF; */
    /* void * value; */

    /* if (limit == 0) limit = 15; */

    /* while (limit--) { */
    /*  value = rand() % rand_seed; */
    /*  list_push(tmp->head, value); */
    /* } */

    /* return tmp; */

    /* this is an old method, i may implenmet it later */
    return NULL;
}

bool_t list_isin(void *value, list_t * list) {
    node_t *tmp = list->head->next;

    while (tmp->next) {
	if (value == tmp->value) {
	    goto RET;
	} else {
	    tmp = tmp->next;
	}
    }

    return false;

  RET:
    return true;
}

list_t *list_distinct(list_t * list) {
    list_t *unique = list_init();
    node_t *ntmp = list->head->next;

    while (ntmp->next) {
	if (!list_isin(ntmp->value, unique)) {
	    list_push(unique->head, ntmp->value);
	}

	ntmp = ntmp->next;
    }

    /*
       while (ntmp && list_find(unique, ntmp->next->value, true)) {
       list_push(unique->head, ntmp->next->value);
       ntmp = ntmp->next;
       }
     */

    return unique;
}

void testing_linked_lists(FILE * stream) {
    list_t *foo = list_random(2 << 5), *unique = list_distinct(foo);

    fputs("\n- testing linked lists\n---------\n", stream);
    list_log(stream, foo);
    list_log(stream, unique);

    list_free(foo);
    list_free(unique);
}
