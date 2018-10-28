/**
 * @file sexpr.c
 *
 * @brief declataions of s-expression and lambda functions
 *
 * functions are devided functions to determine the type such as isnil() or
 * isatom() and also functions to create a s-expression such as sexpr_new()
 * and some handy functions like sexpr_nil() or sexpr_true().
 *
 * and ducntions to handle lambdas such as lambda_new(), lambda_print()
 *
 * @see sexpr.h
 * @see pair.h
 * @see native.h
 */

#include "../include/sexpr.h"
#include "../include/pair.h"
#include "../include/native.h"

/**
 * @brief test if `expr` is `nil`
 *
 * @param expr s-expression
 * @return `true` if `expr` was of type #T_NIL
 */
bool isnil(sexpr_t * expr) {
    return expr == NULL ? false : expr->type == T_NIL;
}

/**
 * @brief test if `expr` is **not** `nil`
 *
 * @param expr s-expression
 * @return `true` if `expr` was of **not** type #T_NIL
 *
 * @note only #T_NIL is considered as `false` anything else is `true`
 */
bool istrue(sexpr_t * expr) {
    return !isnil(expr);
}

/**
 * @brief test if `expr` is an atom such as `1412` or `"string"` or
 * `foo-bar`
 *
 * @param expr s-expression
 * @return `true` if `expr` was of type either #T_NUMBER, #T_STRING
 * or #T_SYMBOL
 *
 * @note #T_NIL is **not** an atom
 */
bool isatom(sexpr_t * expr) {
    return !ispair(expr) && !islambda(expr);
}

/**
 * @brief test if `expr` is a symbol
 *
 * @param expr s-expression
 * @return `true` if `expr` was #T_SYMBOL
 *
 * @note #T_NIL is **not** a symbol
 */
bool issymbol(sexpr_t * expr) {
    return expr == NULL ? false : expr->type == T_SYMBOL;
}

/**
 * @brief test if `expr` is a number
 *
 * @param expr s-expression
 * @return `true` if `expr` was of type #T_NUMBER
 */
bool isnumber(sexpr_t * expr) {
    return expr == NULL ? false : expr->type == T_NUMBER;
}

/**
 * @brief test if `expr` is a string
 *
 * @param expr s-expression
 * @return `true` if `expr` was of type #T_STRING
 */
bool isstring(sexpr_t * expr) {
    return expr == NULL ? false : expr->type == T_STRING;
}

/**
 * @param expr s-expression
 * @return `true` if `expr` was of type #T_PAIR
 *
 * @see pair.h
 */
bool ispair(sexpr_t * expr) {
    return expr == NULL ? false : expr->type == T_PAIR;
}

/**
 * @brief lists is a chain of #T_PAIR of s-expression with a #T_NIL
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
 * @return `true` if `expr` was of type #T_LAMBDA
 *
 * @see sexpr.h
 */
bool islambda(sexpr_t * expr) {
    return expr == NULL ? false : expr->type == T_LAMBDA;
}

/**
 * @param expr s-expression
 * @return `true` if `expr` was a native lambda
 *
 * wgat's wrinbg with moroccan common sense
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
 * @param type s-expression type like #T_NUMBER or #T_SYMBOL
 *
 * @see #SYMBOLIC_EXPRESSION_TYPE
 * @see #SYMBOLIC_EXPRESSION
 *
 * @note if `type` was #T_LAMBDA, it allocates memory for the lambda as well
 */
sexpr_t *sexpr_new(type_t type) {
    sexpr_t *expr = gc_alloc_sexpr();

    expr->type = type;

    if (type == T_LAMBDA)
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
	++length;	   /* this is a pair i.e. no nil at the end */

    return length;
}

/**
 * @brief creates an error s-expression
 *
 * basically calling sexpr_new() passing #T_ERR
 *
 * @return error s-expression
 *
 * @see error.c
 * @note error s-expression is returned after error occurence
 */
sexpr_t *sexpr_err(void) {
    return sexpr_new(T_ERR);
}

/**
 * @brief creating a `nil` s-expression
 *
 * basically calling sexpr_new() passing #T_NIL and initializing its text
 *
 * @return `nil` s-expression
 */
sexpr_t *sexpr_nil(void) {
    sexpr_t *t = sexpr_new(T_NIL);
    t->s = strdup("nil");
    return t;
}

/**
 * @brief creating a symbol s-expresion of `t`
 *
 * basically calling sexpr_new() passing #T_SYMBOL and initializing its
 * text with `"t"`
 *
 * @return error s-expression
 */
sexpr_t *sexpr_true(void) {
    sexpr_t *t = sexpr_new(T_SYMBOL);
    t->s = strdup("t");
    return t;
}

/**
 * @brief allocates memory and initilize a new **native** lambda after
 * calling sexpr_new()
 *
 * @param parent the scope containing the lambda
 * @param args a list of lambda's arguments
 * @param native a native C function
 *
 * @return a s-expression of type #T_LAMBDA
 *
 * @see #LAMBDA_EXPRESSION
 * @see #SYMBOLIC_EXPRESSION
 * @see #NATIVE_LAMBDA
 *
 * @note initializing `isnative` to `true`
 */
sexpr_t *lambda_new_native(scope_t * parent, sexpr_t * args,
			   native_t * func) {
    sexpr_t *lambda = sexpr_new(T_LAMBDA);

    lambda->l->parent = parent;
    lambda->l->args = args;
    lambda->l->isnative = true;
    lambda->l->native = func;

    return lambda;
}

/**
 * @brief allocates memory and initilize a new lambda after calling
 * sexpr_new()
 *
 * @param parent the scope containing the lambda
 * @param args a list of lambda's arguments
 * @param body a s-expression to interpete when calling this lambda
 *
 * @return a s-expression of type #T_LAMBDA
 *
 * @see #LAMBDA_EXPRESSION
 * @see #SYMBOLIC_EXPRESSION
 *
 * @note initializing `isnative` to `false`
 */
sexpr_t *lambda_new(scope_t * parent, sexpr_t * args, sexpr_t * body) {
    sexpr_t *lambda = sexpr_new(T_LAMBDA);

    lambda->l->parent = parent;
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
    case T_PAIR:
	type_str = "CONS-PAIR";
	break;

    case T_NUMBER:
	type_str = "NUMBER";
	break;

    case T_STRING:
	type_str = "STRING";
	break;

    case T_SYMBOL:
	type_str = "SYMBOL";
	break;

    case T_LAMBDA:
	type_str = "LAMBDA";
	isfinished = true;
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

void _sexpr_print(object_t o) {
    sexpr_t *expr = (sexpr_t *) o, *tmp = expr;

    if (expr == NULL) {
	puts("expr was NULL");
	return;
    }

    if (islambda(expr)) {
	lambda_print(expr->l);
	return;
    }

    if (isstring(expr))
	printf("\"%s\"", expr->s);
    else if (issymbol(expr) || isnil(expr))
	printf("%s", expr->s);
    else if (isnumber(expr))
	printf("%lf", expr->n);
    else if (ispair(expr)) {
	putchar('(');

	_sexpr_print(car(expr));

	while (ispair(tmp = cdr(tmp))) {
	    /* a pair be constructed of two pairs
	     * so we should always check that */
	    /* if (ispair(car(tmp)) && cadr(tmp) == NULL) */
	    /*	break; */
	    putchar(' ');
	    _sexpr_print(car(tmp));
	}

	/* if it was just a pair not a list */
	if (!isnil(tmp)) {
	    printf(" . ");
	    _sexpr_print(tmp);
	}

	putchar(')');
    }
}

void sexpr_print(object_t o) {
    if (o == NULL)
	return;
    _sexpr_print(o);
    putchar('\n');
}

void lambda_describe(object_t o) {
    lambda_t *l = o;

    if (l == NULL) {
	puts("lambda was NULL");
	return;
    }

    printf("[%s]", l->gci.ismarked ? "X" : " ");

    /* if (l->parent != NULL) */
    /*	scope_describe(l->parent); */

    if (l->isnative)
	printf("%s - %p\n", l->native->symbol, l->native->func);
    else
	sexpr_describe(l->body);

    if (l->args != NULL)
	sexpr_describe(l->args);
}

void lambda_print(object_t o) {
    lambda_t *l = o;

    if (l == NULL) {
	puts("lambda was NULL");
	return;
    }

    /* printf("[%s] ", l->gci.ismarked ? "X" : " "); */

    /* if (l->parent != NULL) */
    /*	scope_describe(l->parent); */

    if (l->isnative)
	printf("%s @%p\n", l->native->symbol, l->native->func);
    else
	sexpr_print(l->body);

    if (l->args != NULL)
	sexpr_print(l->args);
}
