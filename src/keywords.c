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
 * @brief determines whether a @p expr s-expression is a keyword or not
 *
 * @param expr s-expression
 *
 * @return NULL if the s-expression is not a keyword, or the keyword's
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
 * evaluating them, and since @p expr must be the cdr() of (quote expr),
 * we need tu return the car() which is what we really want, and not
 * @p expr directly because we'll return the terminating nil as well.
 *
 * @param s the contaning scope
 * @param expr the expression to evaluate
 *
 * @return expr without evaluation
 * @note quote is defined as (quote expr)
 */
sexpr_t *eval_quote(scope_t * s, sexpr_t * expr) {
    if (s || true)
	return car(expr);
}

/**
 * @brief define a symbol to hold a sexpr
 *
 * evaluates the cadr() @p expr and then creates a new bind
 * with the result and the symbol in the car() of @p expr
 *
 * @param s the contaning scope
 * @param expr the expression to evaluate
 *
 * @return the defined s-expression
 *
 * @see scope.h
 * @note `defines` are defined as (define symbol expr)
 */
sexpr_t *eval_define(scope_t * s, sexpr_t * expr) {
    sexpr_t *tmp = eval_sexpr(s, cadr(expr));
    vector_push(s->bonds, bond_new(car(expr)->s, tmp));
    return tmp;
}

/**
 * @brief performes a condtional based on the car() of @p expr
 *
 * the condition is the car() of @p expr, if it was true, checked using
 * istrue(), then cadr() is ecaluated, otherwise the caddr() if evalutaed
 * instead.
 *
 * @param s the contaning scope
 * @param expr the expression to evaluate
 *
 * @return the evaluate of expression that satisfies the condition
 *
 * @see sexpr.h
 * @note conditions are done as (if (expr) (true) (false))
 */
sexpr_t *eval_if(scope_t * s, sexpr_t * expr) {
    if (istrue(eval_sexpr(s, car(expr))))
	return eval_sexpr(s, cadr(expr));
    else
	return eval_sexpr(s, caddr(expr));
}


/**
 * @brief creates lambda from @p expr
 *
 * initialize a non native lambda, car() are the args
 * and cadr() is the body
 *
 * @param s the contaning scope
 * @param expr the expression to evaluate
 *
 * @return a lambda s-expression
 *
 * @see sexpr.h
 * @note `lambdas` are defined as (lambda (args) (body))
 */
sexpr_t *eval_lambda(scope_t * s, sexpr_t * expr) {
    return lambda_new(s, car(expr), cadr(expr));
}
