#include "../include/sexpr.h"
#include "../include/pair.h"
#include "../include/scope.h"
#include "../include/native.h"

bool_t isnil(sexpr_t * expr) {
    assert(expr != NULL);
    return expr->type == T_NIL;
}

bool_t isatom(sexpr_t * expr) {
    assert(expr != NULL);
    return issymbol(expr) || isstring(expr) || isnumber(expr);
}

bool_t issymbol(sexpr_t * expr) {
    assert(expr != NULL);
    return expr->type == T_SYMBOL;
}

bool_t isnumber(sexpr_t * expr) {
    assert(expr != NULL);
    return expr->type == T_NUMBER;
}

bool_t isstring(sexpr_t * expr) {
    assert(expr != NULL);
    return expr->type == T_STRING;
}

bool_t islambda(sexpr_t * expr) {
    assert(expr != NULL);
    return expr->type == T_LAMBDA;
}

bool_t ispair(sexpr_t * expr) {
    assert(expr != NULL);
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

void sexpr_describe(object_t o) {
    if (o == NULL) {
	puts("expr was NULL");
	return;
    }

    sexpr_t *expr = (sexpr_t *) o;
    static int ntabs = 0;
    char *type_str = NULL;
    bool_t isfinished = false;

    switch (expr->type) {
    case T_NUMBER:
	type_str = "NUMBER";
	break;			/** 0 -100 0.25 */
    case T_STRING:
	type_str = "STRING";
	break;			/** "anything in between" */
    case T_SYMBOL:
	type_str = "ATOM";
	break;			/** foo foo-bar */

    case T_LAMBDA:		/* TODO: describe lambda */
	type_str = "lambda";
	break;
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

    printf(" [%s] expr: %p, type:%d (%s)\n",
	   expr->gci.ismarked ? "X" : "O", expr, expr->type, type_str);

    if (isfinished) {
	print_tabs(ntabs);
	printf(" ----------------- \n");
	return;
    }

    print_tabs(++ntabs);

    if (isstring(expr) || issymbol(expr)) {
	printf("content: %s\n", expr->s);
    } else if (isnumber(expr)) {
	printf("content: %lf\n", expr->n);
    } else if (ispair(expr)) {
	printf("content: ----------------- \n");
	print_tabs(ntabs);
	sexpr_describe(expr->c->car);
	putchar('\n');
	print_tabs(ntabs);
	sexpr_describe(expr->c->cdr);
    } else if (islambda(expr)) {
	printf("content: ----------------- \n");
	lambda_describe(expr->l);
    }

    --ntabs;
}

void lambda_describe(object_t o) {
    if (o == NULL) {
	puts("lambda was NULL");
	return;
    }

    lambda_t *l = o;

    printf("[%s]", l->gci.ismarked ? "X" : " ");
    scope_describe(l->parent);

    if (l->isnative)
	printf("%s - %p\n", l->native->symbol, l->native->func);
    else
	sexpr_describe(l->body);

    sexpr_describe(l->args);
}
