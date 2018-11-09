#include "repl.h"
#include "lexer.h"
#include "parser.h"
#include "eval.h"
#include "scope.h"
#include "vector.h"
#include "characters.h"

void print_head(void) {
    puts("scmin, Minimal Scheme/Lisp Interpreter\n"
	 "Licenced under GPL v2 by 0x0584\n"
	 "\nstart typing Scheme/Lisp syntax "
	 "or press [RETURN] to exit\n");
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

	if (buffer == NULL || *buffer == '\0') {
	    isfinished = true;
	    goto CLEAN;
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

	printf(" -> "), sexpr_print(tmp);

	vector_free(tokens);

      CLEAN:
	free(buffer);
	gc_setmark_scope(get_global_scope(), true);
	gc_collect(true);
    }
}
