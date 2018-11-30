/**
 * @file sexpr.c
 *
 * @brief declaration of s-expression and lambda functions
 *
 * functions are divided functions to determine the type such as isnil() or
 * isatom() and also functions to create a s-expression such as sexpr_new()
 * and some handy functions like sexpr_nil() or sexpr_true().
 *
 * and functions to handle lambdas such as lambda_new(), lambda_print()
 *
 * @see sexpr.h
 * @see pair.h
 * @see native.h
 */

#include "sexpr.h"
#include "pair.h"
#include "native.h"

bool iserror(sexpr_t * expr) {
    return expr == NULL ? true : expr->type == SCMIN_ERR;
}

/**
 * @brief test if `expr` is `nil`
 *
 * @param expr s-expression
 * @return `true` if `expr` was of type #SCMIN_NIL
 */
bool isnil(sexpr_t * expr) {
    return expr == NULL ? false : expr->type == SCMIN_NIL;
}

/**
 * @brief test if `expr` is **not** `nil`
 *
 * @param expr s-expression
 * @return `true` if `expr` was of **not** type #SCMIN_NIL
 *
 * @note only #SCMIN_NIL is considered as `false` anything else is `true`
 */
bool istrue(sexpr_t * expr) {
    return !isnil(expr);
}

/**
 * @brief test if `expr` is an atom such as `1412` or `"string"` or
 * `foo-bar`
 *
 * @param expr s-expression
 * @return `true` if `expr` was of type either #SCMIN_NUMBER, #SCMIN_STRING
 * or #SCMIN_SYMBOL
 *
 * @note #SCMIN_NIL is **not** an atom
 */
bool isatom(sexpr_t * expr) {
    return !ispair(expr) && !islambda(expr);
}

/**
 * @brief test if `expr` is a symbol
 *
 * @param expr s-expression
 * @return `true` if `expr` was #SCMIN_SYMBOL
 *
 * @note #SCMIN_NIL is **not** a symbol
 */
bool issymbol(sexpr_t * expr) {
    return expr == NULL ? false : expr->type == SCMIN_SYMBOL;
}

/**
 * @brief test if `expr` is a number
 *
 * @param expr s-expression
 * @return `true` if `expr` was of type #SCMIN_NUMBER
 */
bool isnumber(sexpr_t * expr) {
    return expr == NULL ? false : expr->type == SCMIN_NUMBER;
}

/**
 * @brief test if `expr` is a string
 *
 * @param expr s-expression
 * @return `true` if `expr` was of type #SCMIN_STRING
 */
bool isstring(sexpr_t * expr) {
    return expr == NULL ? false : expr->type == SCMIN_STRING;
}

/**
 * @param expr s-expression
 * @return `true` if `expr` was of type #SCMIN_PAIR
 *
 * @see pair.h
 */
bool ispair(sexpr_t * expr) {
    return expr == NULL ? false : expr->type == SCMIN_PAIR;
}

/**
 * @brief lists is a chain of #SCMIN_PAIR of s-expression with a #SCMIN_NIL
 *
 * @param expr s-expression
 * @return `true` if `expr` was a list
 *
 * @see pair.h
 */
bool islist(sexpr_t * expr) {
    return expr == NULL ? false : ispair(expr) && expr->c->islist;
}

/**
 * @brief test if `expr` is a lambda
 *
 * @param expr s-expression
 * @return `true` if `expr` was of type #SCMIN_LAMBDA
 *
 * @see sexpr.h
 */
bool islambda(sexpr_t * expr) {
    return expr == NULL ? false : expr->type == SCMIN_LAMBDA;
}

/**
 * @param expr s-expression
 * @return `true` if `expr` was a native lambda
 *
 * @see sexpr.h
 * @see native.h
 */
bool isnative(sexpr_t * expr) {
    if (islambda(expr))
	return expr->l->isnative;
    else
	return false;
}

/**
 * @brief allocates memory for a new s-expression
 *
 * basically, this is the way to allocate memory for a new s-expression
 * because this function allocates memory using the built-in GC allocation
 *
 * @param type s-expression type like #SCMIN_NUMBER or #SCMIN_SYMBOL
 *
 * @see #SYMBOLIC_EXPRESSION_TYPE
 * @see #SYMBOLIC_EXPRESSION
 *
 * @note if `type` was #SCMIN_LAMBDA, it allocates memory for the lambda as well
 */
sexpr_t *sexpr_new(type_t type) {
    sexpr_t *expr = gc_alloc_sexpr();

    expr->type = type;

    if (type == SCMIN_LAMBDA)
	expr->l = gc_alloc_lambda();

    return expr;
}

/**
 * @brief determines the length of `expr`
 *
 * @param expr s-expression
 *
 * @return the length of `expr`
 *
 * @note `(1 2 (3 4) 6)` is of size `4`
 */
int sexpr_length(sexpr_t * expr) {
    sexpr_t *tmp = expr;
    int length = 0;

    if (expr == NULL || isatom(expr))
	return length;

    while (ispair(tmp))
	++length, tmp = cdr(tmp);

    if (!isnil(tmp))
	++length;		/* this is a pair i.e. no nil at the end */

    return length;
}

/**
 * @brief creates an error s-expression
 *
 * basically calling sexpr_new() passing #SCMIN_ERR
 *
 * @return error s-expression
 *
 * @see error.c
 * @note error s-expression is returned after error occurrence
 */
sexpr_t *sexpr_err(void) {
    return sexpr_new(SCMIN_ERR);
}

/**
 * @brief creating a `nil` s-expression
 *
 * basically calling sexpr_new() passing #SCMIN_NIL and initializing its text
 *
 * @return `nil` s-expression
 */
sexpr_t *sexpr_nil(void) {
    sexpr_t *t = sexpr_new(SCMIN_NIL);
    t->s = strdup("nil");
    return t;
}

/**
 * @brief creating a symbol s-expression of `t`
 *
 * basically calling sexpr_new() passing #SCMIN_SYMBOL and initializing its
 * text with `"t"`
 *
 * @return error s-expression
 */
sexpr_t *sexpr_true(void) {
    sexpr_t *t = sexpr_new(SCMIN_SYMBOL);
    t->s = strdup("t");
    return t;
}

sexpr_t *sexpr_symbol(string_t symbol) {
    sexpr_t *t = sexpr_new(SCMIN_SYMBOL);
    t->s = strdup(symbol);
    return t;
}

sexpr_t *sexpr_number(number_t number) {
    sexpr_t *t = sexpr_new(SCMIN_NUMBER);
    t->n = number;
    return t;
}

sexpr_t *sexpr_string(string_t string) {
    sexpr_t *t = sexpr_new(SCMIN_STRING);
    t->s = strdup(string);
    return t;
}

/**
 * @brief allocates memory and initialize a new **native** lambda after
 * calling sexpr_new()
 *
 * @param args a list of lambda's arguments
 * @param func a native C function
 *
 * @return a s-expression of type #SCMIN_LAMBDA
 *
 * @see #SYMBOLIC_EXPRESSION
 * @see #LAMBDA_EXPRESSION
 * @see #LAMBDA_NATIVE
 *
 * @note initializing `is native` to `true`
 */
sexpr_t *lambda_new_native(sexpr_t * args, nlambda_t * func) {
    sexpr_t *lambda = sexpr_new(SCMIN_LAMBDA);

    lambda->l->args = args;
    lambda->l->isnative = true;
    lambda->l->native = func;

    return lambda;
}

/**
 * @brief allocates memory and initialize a new lambda after calling
 * sexpr_new()
 *
 * @param args a list of lambda's arguments
 * @param body a s-expression to interpret when calling this lambda
 *
 * @return a s-expression of type #SCMIN_LAMBDA
 *
 * @see #LAMBDA_EXPRESSION
 * @see #SYMBOLIC_EXPRESSION
 *
 * @note initializing `isnative` to `false`
 */
sexpr_t *lambda_new(sexpr_t * args, sexpr_t * body) {
    sexpr_t *lambda = sexpr_new(SCMIN_LAMBDA);

    lambda->l->args = args;
    lambda->l->isnative = false;
    lambda->l->body = body;

    return lambda;
}

/**
 * @brief a helper function used by sexpr_describe()
 * @param ntabs number of tabs to print
 */
void print_tabs(int ntabs) {
    int i, j, tab_size = 2;

    for (i = 0; i < ntabs; ++i)
	for (j = 0; j < tab_size; ++j)
	    putchar(' ');
}

/**
 *
 */
void sexpr_describe(object_t o) {
    sexpr_t *expr = (sexpr_t *) o;
    static int ntabs = 0;
    char *type_str = NULL;
    bool isfinished = false;

    if (expr == NULL) {
	puts("expr was NULL");
	return;
    }

    switch (expr->type) {
    case SCMIN_PAIR:
	type_str = "CONS-PAIR";
	break;

    case SCMIN_NUMBER:
	type_str = "NUMBER";
	break;

    case SCMIN_STRING:
	type_str = "STRING";
	break;

    case SCMIN_SYMBOL:
	type_str = "SYMBOL";
	break;

    case SCMIN_LAMBDA:
	type_str = "LAMBDA";
	isfinished = true;
	break;

    case SCMIN_NIL:
	type_str = "NIL";
	isfinished = true;
	break;

    case SCMIN_ERR:
	type_str = "ERROR";
	isfinished = true;
	break;
    }

    printf(" [%s] expr: %p, type:%d (%s)\n",
	   expr->gci.ismarked ? "X" : "O", expr, expr->type, type_str);

    if (islambda(expr))
	lambda_describe(expr->l);

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

void sexpr_print(object_t o) {
    void _sexpr_print(object_t o); /* private header */

    if (o == NULL)
	return;
    else
	_sexpr_print(o);
}

void _sexpr_print(object_t o) {
    sexpr_t *expr = (sexpr_t *) o, *tmp = expr;

    if (expr == NULL){
	putchar('^');
	return;
    }

    if (islambda(expr)) {
	lambda_print(expr->l);
	return;
    }

    /* printf(" [%s] expr: %p, type:%d\n", */
    /*	   expr->gci.ismarked ? "X" : "O", expr, expr->type); */

    if (isstring(expr))
	printf("\"%s\"", expr->s);
    else if (issymbol(expr) || isnil(expr))
	printf("%s", expr->s);
    else if (isnumber(expr))
	printf("%g", expr->n);
    else if (ispair(expr)) {
	putchar('('), _sexpr_print(car(expr));

	while (ispair(tmp = cdr(tmp)))
	    putchar(' '), _sexpr_print(car(tmp));

	/* if it was just a pair not a list */
	if (!isnil(tmp))
	    printf(" . "), _sexpr_print(tmp);

	putchar(')');
    }
}

void lambda_describe(object_t o) {
    lambda_t *l = o;

    if (l == NULL) {
	puts("lambda was NULL");
	return;
    }

    printf("[%s]", l->gci.ismarked ? "X" : " ");

    if (l->isnative)
	printf("%s - %p\n", l->native->symbol, l->native->func);
    else
	sexpr_describe(l->body);

    if (l->args != NULL)
	sexpr_describe(l->args);
}

void lambda_print(object_t o) {
    lambda_t *l = o;

    if (l == NULL){
	putchar('^');
	return;
    }

    putchar('(');

    if (l->args != NULL)
	printf("%s: ", "args"), sexpr_print(l->args),
	    printf("%s", " - ");

    if (l->isnative)
	printf("%s @%p", l->native->symbol, l->native->func);
    else
	printf("%s: ", "body"), sexpr_print(l->body);

    putchar(')');
}
