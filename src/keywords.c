/**
 * @file keywords.c
 *
 * @brief declatation of the C functions that are accosiated
 * with each keyword. the current keywords
 *
 * there's also a static struct that holds everything up, which is used
 * by iskeyword()
 *
 * @todo check for errors within the eval_x() functions too
 */

#include "../include/eval.h"
#include "../include/scope.h"
#include "../include/vector.h"
#include "../include/pair.h"

/**
 * @brief static array of predefined Scheme keywords
 */
static keyword_t kwd[] = {
    {"quote", eval_quote},
    {"define", eval_define},
    {"if", eval_if},
    {"lambda", eval_lambda},
    {NULL, NULL}
};

/**
 * @brief determines whether a `expr` s-expression is a keyword or not
 *
 * @param expr s-expression
 *
 * @return `NULL` if the `expr` is not a keyword, or the keyword's
 * correspondant function otherwise
 */
k_func iskeyword(sexpr_t * expr) {
    int i;

    if (!expr || !issymbol(expr))
	return NULL;		/* not a symbol */

    for (i = 0; kwd[i].keyword; ++i)
	/* looking for the keyword */
	if (!strcmp(expr->s, kwd[i].keyword))
	    return kwd[i].func;

    return NULL;
}


/**
 * @brief returns the expression as it is
 *
 * quote gives the ability to just pass s-expression without
 * evaluating them, and since `expr` must be the cdr() of `'expr`,
 * we need tu return the car() which is what we really want, and not
 * `expr` directly because we'll return the terminating nil as well.
 *
 * @param scope the contaning scope
 * @param expr the expression to evaluate
 *
 * @return expr without evaluation
 * @note quote is defined as (quote expr)
 */
sexpr_t *eval_quote(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, !sexpr_length(expr));

    if (err_log())
	return sexpr_err();

    if (scope || true)
	return car(expr);
}

/**
 * @brief define a symbol to hold a sexpr
 *
 * evaluates the cadr() `expr` and then creates a new bind
 * with the result and the symbol in the car() of `expr`
 *
 * @param scope the contaning scope
 * @param expr the expression to evaluate
 *
 * @return the defined s-expression
 *
 * @see scope.h
 * @note `defines` are defined as (define symbol expr)
 */
sexpr_t *eval_define(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, sexpr_length(expr) != 2);
    err_raise(ERR_ARG_TYPE, !issymbol(car(expr)));

    if (err_log())
	return sexpr_err();

    sexpr_t *evaled = eval_sexpr(scope, cadr(expr));
    sexpr_t *symbol = car(expr);

    /* vector_print(scope->bonds); */
    scope_push_bond(scope, bond_new(symbol->s, evaled));

    return symbol;
}

/**
 * @brief performes a condtional based on the car() of `expr`
 *
 * the condition is the car() of `expr`, if it was `true`, checked using
 * istrue(), then cadr() is ecaluated, otherwise the caddr() if evalutaed
 * instead.
 *
 * @param scope the contaning scope
 * @param expr the expression to evaluate
 *
 * @return the evaluate of expression that satisfies the condition
 *
 * @see sexpr.h
 * @note conditions are done as `(if (expr) (foo) (bar))`. `foo` is
 * evaluated when `expr` is not `nil`, otherwise egvaluate `bar`
 */
sexpr_t *eval_if(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, sexpr_length(expr) != 3);

    if (err_log())
	return sexpr_err();

    if (istrue(eval_sexpr(scope, car(expr))))
	return eval_sexpr(scope, cadr(expr));
    else
	return eval_sexpr(scope, caddr(expr));
}


/**
 * @brief creates lambda from `expr`
 *
 * initialize a non native lambda, car() are the args and cadr() is
 * the body
 *
 * @param scope the contaning scope
 * @param expr the expression to evaluate
 *
 * @return a lambda s-expression
 *
 * @see sexpr.h
 * @note `lambdas` are defined as `(lambda (args) (body))`
 */
sexpr_t *eval_lambda(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, !sexpr_length(expr));

    if (err_log())
	return sexpr_err();

    return lambda_new(scope, car(expr), cadr(expr));
}
