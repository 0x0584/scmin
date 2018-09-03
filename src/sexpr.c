#include "../include/sexpr.h"
#include "../include/pair.h"
#include "../include/scope.h"
#include "../include/native.h"

bool isnil(sexpr_t * expr) {
    assert(expr != NULL);
    return expr->type == T_NIL;
}

bool isatom(sexpr_t * expr) {
    assert(expr != NULL);
    return issymbol(expr) || isstring(expr) || isnumber(expr);
}

bool issymbol(sexpr_t * expr) {
    assert(expr != NULL);
    return expr->type == T_SYMBOL;
}

bool isnumber(sexpr_t * expr) {
    assert(expr != NULL);
    return expr->type == T_NUMBER;
}

bool isstring(sexpr_t * expr) {
    assert(expr != NULL);
    return expr->type == T_STRING;
}

bool islambda(sexpr_t * expr) {
    assert(expr != NULL);
    return expr->type == T_LAMBDA;
}

bool ispair(sexpr_t * expr) {
    assert(expr != NULL);
    return expr->type == T_PAIR;
}

sexpr_t *sexpr_new(type_t type) {
    sexpr_t *expr = gc_alloc_sexpr();

    expr->type = type;

    if (type == T_LAMBDA)
	expr->l = gc_alloc_lambda();

    return expr;
}

sexpr_t *lambda_new_native(scope_t * parent, sexpr_t * args,
			   native_t * func) {
    sexpr_t *lambda = sexpr_new(T_LAMBDA);

    lambda->l->parent = parent;
    lambda->l->args = args;
    lambda->l->isnative = true;
    lambda->l->native = func;

    return lambda;
}

sexpr_t *lambda_new(scope_t * parent, sexpr_t * args, sexpr_t * body) {
    sexpr_t *lambda = sexpr_new(T_LAMBDA);

    lambda->l->parent = parent;
    lambda->l->args = args;
    lambda->l->isnative = false;
    lambda->l->body = body;

    return lambda;
}

void print_tabs(int ntabs) {
    int i, j, tab_size = 4;

    for (i = 0; i < ntabs; ++i)
	for (j = 0; j < tab_size; ++j)
	    putchar(' ');
};

void sexpr_describe(object_t o) {
    if (o == NULL) {
	puts("expr was NULL");
	return;
    }

    sexpr_t *expr = (sexpr_t *) o;
    static int ntabs = 0;
    char *type_str = NULL;
    bool isfinished = false;

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

    if (isstring(expr) || issymbol(expr))
	printf("content: %s\n", expr->s);
    else if (isnumber(expr))
	printf("content: %lf\n", expr->n);
    else if (ispair(expr)) {
	printf("content: ----------------- \n");
	print_tabs(ntabs);
	sexpr_describe(expr->c->car);
	putchar('\n');
	print_tabs(ntabs);
	sexpr_describe(expr->c->cdr);
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

    if (l->parent != NULL)
	scope_describe(l->parent);

    if (l->isnative)
	printf("%s - %p\n", l->native->symbol, l->native->func);
    else
	sexpr_describe(l->body);

    sexpr_describe(l->args);
}
