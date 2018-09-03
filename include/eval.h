#ifndef _SCMIN_EVAL_H
#  define _SCMIN_EVAL_H

#  include "types.h"
#  include "gc.h"

#  include "sexpr.h"

/* (define symbol sexpr) */
sexpr_t *eval_define(scope_t *, ...);
/* (if (sexpr_condition)
 *   (sexpr_true)
 *   (sexpr_false)) */
sexpr_t *eval_if(scope_t *, ...);

sexpr_t *eval(scope_t * s, sexpr_t * expr);

#  if EVALUATOR_DEBUG == DBG_ON
void eval_testing();
#  endif
#endif				/* _SCMIN_EVAL_H */
