/**
 * @mainpage A Minimal Lisp/Scheme Interpreter
 *
 * @section intro_sec About
 *
 * This is a basic Lisp (technically Scheme) interpreter that was written
 * in C for educational purposes, programming for me is a passion, sort of
 * art, thus; this is just for fun and learning new stuff. and what could
 * be more wonderful than writing your own lisp interpreter.
 *
 * for the purpure learning more, Lisp and Scheme are the same for the moment,
 * so the code written in here is a mix between teh two languages.
 *
 * there might be an undefined number of *bugs* in here, i'm aware of some
 * and some remain unknown.

 * @section run_sec Running the program
 *
 * nothing more simple than a make command:
 *
 * ```shell
 * $ make && ./scmin
 * ```
 * @subsection debug Debugging the program
 *
 * You might want to turn debigging on for each phase, for example to
 * turn evaluation debbuging, set the #DEBUG_EVALUATOR to DEBUG_ON and
 * recompile
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

    gc_collect(true);
    gc_log(true);
}

int main(int argc, char **argv) {
    if (argc == 1 && argv[0]) {
	/* just to dismiss the warnings for now */
    }

    gc_init();
    scmin_init();

    /* repl(); */

    gc_clean();

    return EXIT_SUCCESS;
}
