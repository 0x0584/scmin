/*
 * this would split a text input into tokens, so that the parser
 * could create a parse-tree
 */
#include "../include/lexer.h"

vector_t *read_token(string_t code) {
    return NULL;
}

string_t stream_as_string(FILE *stream) {
    return "";
}

vector_t *read_stream_token(FILE *stream) {
    return read_token(stream_as_string(stream));
}
