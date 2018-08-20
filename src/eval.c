#include "../include/eval.h"
#include "../include/context.h"
#include "../include/vector.h"

sexpr_t *eval(scope_t *s, sexpr_t *expr) {
    context_t *context = context_init(s, expr);

    sexpr_t *tmp = NULL;

    context_add_local(context, tmp);

    return tmp;
}
