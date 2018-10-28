#ifndef _SCMIN_EVAL_H
#  define _SCMIN_EVAL_H

/**
 * @file eval.h
 *
 * @brief declaration of evaluation-related functions
 *
 * @see src/keywords.c
 */

#  include "types.h"
#  include "gc.h"

#  include "sexpr.h"

/**
 * @brief evaluate a @p expr within a given @p scope and return
 * the evaluated s-expression
 *
 * this function may call it self recursively in order to evaluate
 * inner s-expressions.
 *
 * @param scope the contaning scope
 * @param expr a s-expreesion to evaluate
 *
 * @return the evaluated s-expression
 *
 * @note this is a recursive function
 */
sexpr_t *eval_sexpr(scope_t * s, sexpr_t * expr);


/**
 * @brief just like eval_sexpr(), but with a vector of s-expressions
 * instead of a single one
 *
 * calling eval_sexpr() passing the global scope along with each
 * expression in @p sexprs vector. results are saved into a new vector
 *
 * @param sexprs a vector of s-expressions
 *
 * @return a vector of the evaluated s-expressions
 */
vector_t *eval_sexprs(vector_t * exprs);

/**
 * @brief keyword function prototype
 *
 * keywords need a scope that holds data, and also a s-expression
 * to operate on
 *
 * @param s a container scope
 * @param expr expression to operate on
 *
 * @return return of the keyword operation as s-expression
 *
 * @note keywords may need to evaluate expr and
 */
typedef sexpr_t *(*k_func) (scope_t * s, sexpr_t * expr);

/**
 * @brief pairing keywords with their corespondent C functions.
 *
 * @note those functions shall not be called directly
 */
struct KEYWORD {
    /**
     * @brief the Lisp/Scheme keyword such as quote or if
     */
    string_t keyword;
    /**
     * @brief the C function that is related to the keyword
     */
    k_func func;
};

/**
 * @brief determines whether a @p expr s-expression is a keyword or not
 *
 * @param expr s-expression
 *
 * @return NULL if the s-expression is not a keyword, or the keyword's
 * correspondant function otherwise
 */
k_func iskeyword(sexpr_t * expr);


/**
 * @brief returns the expression as it is
 *
 * @param s the contaning scope
 * @param expr the expression to evaluate
 *
 * @return expr without evaluation
 * @note `quote` is defined as (quote expr)
 */
sexpr_t *eval_quote(scope_t * s, sexpr_t * expr);

/**
 * @brief define a symbol to hold a sexpr
 *
 * @param s the contaning scope
 * @param expr the expression to evaluate
 *
 * @return the defined s-expression
 *
 * @see scope.h
 * @note `defines` are defined as (define symbol expr)
 */
sexpr_t *eval_define(scope_t *, sexpr_t * expr);

/**
 * @brief performes a condtional based on the car() of @p expr
 *
 * @return the evaluate of expression that satisfies the condition
 *
 * @see sexpr.h
 * @note conditions are done as (if (expr) (true) (false))
 */
sexpr_t *eval_if(scope_t *, sexpr_t * expr);


/**
 * @brief creates lambda from @p expr
 *
 * @param s the contaning scope
 * @param expr the expression to evaluate
 *
 * @return a lambda s-expression
 *
 * @see sexpr.h
 * @note `lambdas` are defined as (lambda (args) (body))
 */
sexpr_t *eval_lambda(scope_t * s, sexpr_t * expr);

void eval_testing();
#endif				/* _SCMIN_EVAL_H */
