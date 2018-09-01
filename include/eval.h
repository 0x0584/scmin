#ifndef _SCMIN_EVAL_H
#  define _SCMIN_EVAL_H

#  include "types.h"
#  include "gc.h"

#  include "sexpr.h"

#  define OUTPUT_SIZE (2<<10)

typedef enum KEYWORD {
    K_NOT_KEYWORD = 0,

    K_DEFINE,
    K_IF,
    K_AND,
    K_OR,
    K_NOT
} keyword_t;

keyword_t iskeyword(sexpr_t * expr);
sexpr_t *eval_keyword(keyword_t k, sexpr_t * expr);

/* (define symbol 's-expr) */
sexpr_t *eval_define(scope_t *, sexpr_t *);
/* (if (condition) (true) (false)) */
sexpr_t *eval_if(scope_t *, sexpr_t *);
/* (or s-exprs) */
sexpr_t *eval_or(scope_t *, sexpr_t *);
/* (not s-expr) */
sexpr_t *eval_not(scope_t *, sexpr_t *);
/* (and s-exprs) */
sexpr_t *eval_and(scope_t *, sexpr_t *);

sexpr_t *eval(scope_t * s, sexpr_t * expr);

#  if EVALUATOR_DEBUG == DBG_ON
void eval_testing();
#  endif
#endif				/* _SCMIN_EVAL_H */
