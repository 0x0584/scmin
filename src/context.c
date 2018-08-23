#include "../include/context.h"

static scope_t *global;
static vector_t *eval_stack;

context_t *context_init(scope_t *s, sexpr_t *expr);
void context_reset(context_t c, scope_t *s, sexpr_t*expr);
context_t *context_current(void);
void context_add_local(context_t *c, sexpr_t *expr);
