#ifndef _SCMIN_EVAL_H
#  define _SCMIN_EVAL_H

#  include "types.h"
#  include "gc.h"

#  include "sexpr.h"

#  define OUTPUT_SIZE (2<<10)

typedef struct EVAL_RESULT {
    string_t str;
    sexpr_t *result;
} result_t;

sexpr_t *eval(scope_t * s, sexpr_t * expr);

#  if EVALUATOR_DEBUG == DBG_ON
void eval_testing();
#  endif
#endif				/* _SCMIN_EVAL_H */
