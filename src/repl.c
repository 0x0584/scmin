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
 *
 * @todo stop the loop after hitting ^D not RETURN after finishing
 * the debugging process
 */

#include "characters.h"
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

void repl(scope_t * scope) {
    vector_t *tokens = NULL;
    string_t buffer = NULL;
    sexpr_t *tmp = NULL;
    bool isfinished = false;

    print_head();

    while (!isfinished) {
	printf("%s ", REPL_PROMPT);

	buffer = stdin_as_string();

	if (buffer == NULL) {
	    isfinished = true, putchar('\n');
	    goto CLEAN;
	} else if (*buffer == '\0') {
	    putchar('\n');
	    continue;
	}

	tokens = read_tokens(buffer);

#if REPL_DEBUG == DBG_ON
	vector_print(tokens);
	puts("tokens done");
#endif

	tmp = parse_sexpr(tokens);

#if REPL_DEBUG == DBG_ON
	sexpr_print(tmp);
	puts("parse done");
#endif

	tmp = eval_sexpr(scope, tmp);

#if REPL_DEBUG == DBG_ON
	sexpr_print(tmp);
	puts("eval done");
#endif

	printf(" -> "), sexpr_print(tmp), putchar('\n');

	vector_free(tokens);

      CLEAN:
	free(buffer);
	gc_setmark_scope(get_global_scope(), true);
	gc_collect(true);
    }
}
