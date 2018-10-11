#include "../include/eval.h"
#include "../include/parser.h"
#include "../include/lexer.h"
#include "../include/vector.h"

void eval_testing() {
    vector_t *v = NULL, *w = NULL, *x = NULL;

    v = read_stream_tokens("examples/test.scm");
    w = parse_sexprs(v);
    x = eval_sexprs(w);

    vector_free(w);
    vector_free(x);
    vector_free(v);

    gc_collect(true);
}
