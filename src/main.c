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
#include "../include/vector.h"

int main(int argc, char **argv) {
    if (argc == 1 && argv[0]) {

    }

    vector_testing();
    return EXIT_SUCCESS;
}
