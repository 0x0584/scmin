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

sexpr_t *eval_sexpr(scope_t * s, sexpr_t * expr);
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

k_func eval_keyword(sexpr_t * expr);
sexpr_t *eval_quote(scope_t * s, sexpr_t * expr);
sexpr_t *eval_define(scope_t *, sexpr_t * expr);
sexpr_t *eval_if(scope_t *, sexpr_t * expr);
sexpr_t *eval_lambda(scope_t * s, sexpr_t * expr);
sexpr_t *eval_set(scope_t * scope, sexpr_t * expr);
sexpr_t *eval_setq(scope_t * scope, sexpr_t * expr);
sexpr_t *eval_undef(scope_t * scope, sexpr_t * expr);
sexpr_t *eval_eval(scope_t * scope, sexpr_t * expr);

void eval_testing();
#endif				/* _SCMIN_EVAL_H */
