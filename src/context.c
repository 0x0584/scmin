#include "../include/context.h"
#include "../include/scope.h"
#include "../include/vector.h"

context_t *context_init(scope_t *scope, sexpr_t *expr) {
    context_t *ctx = gc_alloc_context();

    ctx->scope = scope;
    ctx->sexpr = expr;

    return ctx;
}

void context_reset(context_t c, scope_t *s, sexpr_t*expr);
context_t *context_current(void);
void context_add_local(context_t *c, sexpr_t *expr);
void context_describe(object_t o) {
    if (o == NULL) return;
}
