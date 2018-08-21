/*
 * this is the main entery point, basically would intialize the
 * environment, then check if there are any sent-arguments to process.
 *
 *   + if so, it would evaluate them then quit with success (or failure
 *     if there was an error while the evaluation).
 *
 *   + if no argument was sent, a repl-system would set up on infinity
 *     loop (until EOF is sent by the user)
 */
#include "../include/main.h"
#include "../include/gc.h"

#include "../include/vector.h"
#include "../include/lexer.h"
#include "../include/parser.h"

int main(int argc, char **argv) {
    if (argc == 1 && argv[0]) {
	/* just to dimiss the warnings for now */
    }

    gc_init();

#if VECTOR_DEBUG == DBG_ON	/* there's a bug in the vector somewhere! */
    vector_testing();
#endif

#if LEXER_DEBUG == DBG_ON
    lexer_testing();
#endif

#if PARSER_DEBUG == DBG_ON
    parser_testing();
#endif

    gc_collect();

    return EXIT_SUCCESS;
}

void raise_error(FILE * stream, string_t errmsg) {
    fputs(errmsg, stream);
}
