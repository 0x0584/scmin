#include "../include/parser.h"
#include "../include/lexer.h"
#include "../include/vector.h"
#include "../include/sexpr.h"

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
