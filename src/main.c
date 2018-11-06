/**
 * @mainpage A Minimal Lisp/Scheme Interpreter
 */

/**
 * @brief this is the main entery point, basically would intialize the
 * environment, then check if there are any sent-arguments to process.
 *
 *   + if so, it would evaluate them then quit with success (or failure
 *     if there was an error while the evaluation).
 *
 *   + if no argument was sent, a repl-system would set up on infinity
 *     loop (until EOF is sent by the user)
 */

#include "main.h"
#include "gc.h"
#include "vector.h"
#include "sexpr.h"
#include "lexer.h"
#include "parser.h"
#include "eval.h"
#include "repl.h"

int main(int argc, char **argv) {
    if (argc == 1 && argv[0]) {
	/* just to dimiss the warnings for now */
    }

    gc_init();

    /* vector_testing(); */
    /* lexer_testing(); */
    /* parser_testing(); */
    /* eval_testing(); */
    repl_testing();
    /* gc_debug_memory(); */

    gc_clean();
    /* err_clean(); */

    return EXIT_SUCCESS;
}
