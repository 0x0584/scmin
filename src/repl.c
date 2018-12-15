/**
 * @file repl.c
 *
 * @brief contains definitions of Read Eval Print Loop functionalities
 *
 * @details takes input from stdin and then pass it through the evaluation
 * process mainly read_token() to READ the tokens out of the string and
 * then parse_sexpr() passing the collected tokens. after that we get a
 * parsed s-expression, and then call eval_sexpr() to EVALuate it, PRINTing
 * the result and LOOP.
 *
 * @see lexer.c
 * @see parser.c
 * @see eval.c
 */

#include "chars.h"
#include "vector.h"
#include "scope.h"

#include "lexer.h"
#include "parser.h"
#include "eval.h"
#include "repl.h"

void print_head(void) {
    puts("scmin, Minimal Scheme/Lisp Interpreter (0x0584)\n"
	 "scmin comes with ABSOLUTELY NO WARRANTY; it is free software\n"
	 "you can modify it under terms of GNU GPL (v2) (see LICENCE)\n"
	 "\nstart typing Scheme/Lisp syntax or press ^D to exit\n");
}

void repl(void) {
    vector_t *tokens = NULL;
    string_t buffer = NULL;
    scope_t *scope = get_global_scope();
    sexpr_t *tmp = NULL;
    bool isfinished = false;
    int count = 0;

    print_head();

    while (!isfinished) {
	printf("(%d) %s ", ++count, REPL_PROMPT);

	buffer = stdin_as_string();

	if (buffer == NULL) {
	    isfinished = true, putchar('\n');
	    goto CLEAN;
	} else if (*buffer == '@') {
	    free(buffer);
	    putchar('\n');
	    continue;
	}

	if (!(tokens = read_tokens(buffer)))
	    goto CLEAN;

#if DEBUG_REPL == DEBUG_ON
	vector_print(tokens);
	puts("tokens done");
#endif

	tmp = parse_sexpr(tokens);

#if DEBUG_REPL == DEBUG_ON
	sexpr_print(tmp);
	puts("parse done");
#endif

	tmp = eval_sexpr(scope, tmp);

#if DEBUG_REPL == DEBUG_ON
	sexpr_print(tmp);
	puts("eval done");
#endif

	printf(" -> "), sexpr_print(tmp), putchar('\n');

	vector_free(tokens);

      CLEAN:
	free(buffer);
	gc_collect(false);
	gc_log(true);
    }
}
