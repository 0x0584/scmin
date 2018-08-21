#include "../include/sexpr.h"
#include "../include/pair.h"

bool_t isnil(sexpr_t *expr) {
    return expr->type == T_NIL;
}

bool_t isatom(sexpr_t *expr) {
    return expr->type == T_ATOM;
}

bool_t isnumber(sexpr_t *expr) {
    return expr->type == T_NUMBER;
}

bool_t isstring(sexpr_t *expr) {
    return expr->type == T_STRING;
}

bool_t isboolean(sexpr_t *expr) {
    return expr->type == T_BOOLEAN || isnil(expr);
}

bool_t ispair(sexpr_t *expr) {
    return expr->type == T_PAIR;
}

sexpr_t *sexpr_new(type_t type) {
    sexpr_t *expr = gc_alloc_sexpr();

    expr->type = type;

#if GC_DEBUG == DBG_ON
    /* printf("%d", type); */
    /* gc_debug_memory(); */
#endif

    return expr;
}

void sexpr_free(sexpr_t * expr);

void print_tabs(int ntabs) {
    int i, j, tab_size = 4;

    for (i = 0; i < ntabs; ++i) {
	for (j = 0; j < tab_size; ++j) {
	    putchar(' ');
	}
    }
};

void sexpr_describe(sexpr_t * expr) {
    static int ntabs = 0;
    char *type_str = NULL;
    type_t type;


    bool_t isfinished = false;

    switch (type = expr->type) {
    case T_NUMBER:
	type_str = "NUMBER";
	break;			/** 0 -100 0.25 */
    case T_BOOLEAN:
	type_str = "BOOLEAN";
	break;			/** #t #f t nil */
    case T_STRING:
	type_str = "STRING";
	break;			/** "anything in between" */
    case T_ATOM:
	type_str = "ATOM";
	break;			/** foo foo-bar */
    case T_PAIR:
	type_str = "CONS-PAIR";
	break;
    case T_NIL:
	type_str = "NIL";
	isfinished = true;
	break;
    case T_ERR:
	type_str = "ERROR";
	isfinished = true;
	break;
    }

    printf("expr: %p, type:%d (%s)\n", expr, type, type_str);

    if (isfinished){
	return;
    }

    print_tabs(++ntabs);

    if (type == T_STRING || type == T_ATOM) {
	printf("content: %s\n", expr->v.s);
    } else if (type == T_NUMBER) {
	printf("content: %lf\n", expr->v.n);
    } else if (type == T_BOOLEAN) {
	printf("content: %s\n", expr->v.b ? "TRUE" : "FALSE");
    } else if (type == T_PAIR) {
	printf("content: \n");
	print_tabs(ntabs);
	sexpr_describe(expr->v.c->car);
	putchar('\n');
	print_tabs(ntabs);
	sexpr_describe(expr->v.c->cdr);
    }
    --ntabs;
}
