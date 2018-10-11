#include "../include/lexer.h"
#include "../include/vector.h"

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
