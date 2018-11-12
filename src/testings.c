#include "vector.h"

#include "lexer.h"
#include "parser.h"
#include "eval.h"
#include "scope.h"
#include "repl.h"

void debug_int(FILE * stream, object_t o) {
    if (!o)
	return;

    int *i = o;

    fprintf(stream, " %d ", *i);
}

void print_int(object_t o) {
    debug_int(stdout, o);
}

void free_int(object_t o) {
    if (o)
	return;
}

void vector_testing(void) {
    int i, size = 20, tab[size];
    FILE *foo = fopen("2.txt", "w"),
	*bar = fopen("3.txt", "w"), *baz = fopen("1.txt", "w");
    srand(time(NULL));

    vector_t *v = vector_new(free_int, print_int, NULL);
    vector_set_debug(v, debug_int);

    for (i = 0; i < size; ++i) {
	tab[i] = i + 1;
    }

    for (i = 0; i < size; ++i) {
	vector_push(v, &tab[i]);
    }

    puts("after pushing all objects...");
    vector_debug(baz, v);

    for (i = 0; i < v->size; ++i) {
	if (rand() % 2 == 0)
	    vector_set(v, i, NULL);
    }

    puts("after removing mod 3 elements");
    vector_debug(foo, v);

    puts("after compacting the vector");
    vector_compact(v);
    vector_debug(bar, v);

    puts("freeing the vector");
    vector_free(v);

    fclose(foo);
    fclose(bar);
    fclose(baz);
}

void lexer_testing(void) {
    char *exprs[] = {
	"(\"this is a string\")	 ",
	"(+ 4512 (* 45 2054))",
	"(car \'((foo bar) (fuzz buzz)))",
	"    ; this is cool\n(bar baz)"
    };

    int i, size = sizeof(exprs) / sizeof(exprs[0]);

    vector_t *tokens;

    for (i = 0; i < size; ++i) {
	printf("\n-------------\n%s\n-------------\n", exprs[i]);
	puts("get the tokens");
	tokens = read_tokens(exprs[i]);

	puts("printing the tokens");
	vector_debug(stdout, tokens);

	puts("free the tokens");
	vector_free(tokens);
	puts("%");
	/* getchar(); */
    }
}

void parser_testing(void) {
    string_t exprs[] = {
	"(+ 11111 (* 22222 33333))",
	"(define bar '(* 22222 33333))",
	"(define bar 'b)",
	"(quote (a b c))" "(quote a)" "	 ; this is cool\n(bar baz)",
	"(define square (lambda (n) (* n n)))",
	"(\"this is a string\")	     "
    };

    int i, size = sizeof(exprs) / sizeof(exprs[0]);
    vector_t *v = NULL;
    sexpr_t *expr = NULL;

    for (i = 0; i < size; ++i) {
	printf("\n + parsing %s\n", exprs[i]);

	v = read_tokens(exprs[i]);

	puts("\n + list of tokens");
	vector_print(v);
	puts("-----------\n");

	expr = parse_sexpr(v);
	puts("\n + parsed expression");
	sexpr_print(expr);
	puts(" ================== ================= ================= ");

	vector_free(v);
	gc_collect(true);
    }
}

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

void repl_testing(void) {
    repl(get_global_scope());
}
