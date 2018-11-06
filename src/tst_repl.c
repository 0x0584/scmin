#include "repl.h"
#include "scope.h"

void repl_testing(void) {
    repl(get_global_scope());
}
