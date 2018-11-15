/**
 * @mainpage A Minimal Lisp/Scheme Interpreter
 */

/**
 * @file main.c
 *
 * @brief this is the main entry point, basically would initialize the
 * environment, then check if there are any sent-arguments to process.
 *
 *   + if so, it would evaluate them then quit with success (or failure
 *     if an error occurs while evaluating).
 *
 *   + if no argument was sent, a repl-system would set up on infinity
 *     loop
 */

#include "main.h"
#include "gc.h"

#include "vector.h"
#include "sexpr.h"
#include "scope.h"

#include "lexer.h"
#include "parser.h"
#include "eval.h"
#include "repl.h"

void scmin_init(void) {
    vector_t *v = NULL, *w = NULL, *x = NULL;

    v = read_stream_tokens(STD_SCHEME_LIB);
    w = parse_sexprs(v);
    x = eval_sexprs(w);

    vector_free(x);
    vector_free(w);
    vector_free(v);

    gc_setmark_scope(get_global_scope(), true);
    gc_collect(true);
}

int main(int argc, char **argv) {
    if (argc == 1 && argv[0]) {
	/* just to dismiss the warnings for now */
    }

    gc_init();
    scmin_init();

    repl();

    gc_clean();

    return EXIT_SUCCESS;
}
