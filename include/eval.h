#ifndef _SCMIN_EVAL_H
#  define _SCMIN_EVAL_H

/**
 * @file eval.h
 *
 * this header contains the definition of evaluation related methods,
 * which are eval_sexpr() and any other eval_X() method.
 */

#  include "types.h"
#  include "gc.h"

#  include "sexpr.h"

/**
 * evaluate a @p expr within a given @p scope and return the evaluated
 * s-expression, this function may call it self recursively in order
 * to evaluate inner s-expressions.
 *
 * @param scope the contaning scope
 * @param expr a s-expreesion to evaluate
 *
 * @return the evaluated s-expression
 */
sexpr_t *eval_sexpr(scope_t * s, sexpr_t * expr);

sexpr_t *eval_define(scope_t *, sexpr_t * expr);
sexpr_t *eval_if(scope_t *, sexpr_t * expr);
sexpr_t *eval_quote(scope_t * s, sexpr_t * expr);

void eval_testing();
#endif				/* _SCMIN_EVAL_H */
