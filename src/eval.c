#include "../include/eval.h"
#include "../include/scope.h"
#include "../include/context.h"
#include "../include/native.h"

#include "../include/pair.h"
#include "../include/vector.h"
#include "../include/characters.h"

bool_t iskeyword();
void eval_keyword();

/* TODO: handle erros i a more sofisticated way */
sexpr_t *eval(scope_t * s, sexpr_t * expr) {
    sexpr_t *operator, *tail = NULL, *args, *tmp = expr;
    sexpr_t *sexpr_nil = sexpr_new(T_NIL);

    /* ======================================================  
     *         handling operator in (operator sexps)
     * ======================================================  
     * IF iskeyword(expr) THEN
     *     eval keyword passing the cdr to the propere method
     *     
     * ELSE IF issymbol(expr) THEN
     *     resolve (somwhow) the bond whitin the scope
     *     IF bond not resolved THEN
     *         raise_error()
     *         
     * ELSE IF !ispair(expr) THEN
     *     return expr
     * ======================================================  
     */

    if (!(operator = eval(s, car(expr))))
	return car(expr);

    /* ======================================================  
     *                   getting arguments
     * ======================================================  
     * WHILE we still have pairs DO
     *     evaluate each argument's value
     *     make a list of values
     * ======================================================  
     */

    while (ispair(tmp = cdr(tmp))) {
	if (!tail)
	    tail = cons(eval(s, car(tmp)), sexpr_nil);
	else
	    args = cons(eval(s, car(tmp)), sexpr_nil);

	tail = args;
    }

    /* ======================================================  
     *                 applying the operator
     * ======================================================  
     * IF isnative(operator) THEN
     *     return operator->func(args)
     * ELSE
     *     create a new child-scope
     *     bind lambda's args to the child-scope
     *     WHILE body not nil DO
     *         evaluate each s-expr using child-scope
     *         return last expression's value
     * ======================================================  
     */

    return NULL;
}

/* (define symbol 's-expr) */
sexpr_t *eval_define(scope_t *, sexpr_t *);
/* (if (condition) (true) (false)) */
sexpr_t *eval_if(scope_t *, sexpr_t *);
/* (or s-exprs) */
sexpr_t *eval_or(scope_t *, sexpr_t *);
/* (not s-expr) */
sexpr_t *eval_not(scope_t *, sexpr_t *);
/* (and s-exprs) */
sexpr_t *eval_and(scope_t *, sexpr_t *);


#if EVALUATOR_DEBUG == DBG_ON

#  include "../include/lexer.h"
#  include "../include/parser.h"

void eval_testing() {
    string_t exprs[] = {
	"(+ 11111 (* 22222 33333))",
	"    ; this is cool\n(bar baz)",
	"(\"this is a string\")	 "
    };

    int i, size = sizeof(exprs) / sizeof(exprs[0]);
    vector_t *v = NULL;
    sexpr_t *expr = NULL;
    scope_t *gs = scope_init(NULL);
    
    for (i = 0; i < size; ++i) {
	printf("\n + parsing %s\n", exprs[i]);

	v = read_tokens(exprs[i]);

	puts("\n + list of tokens");
	vector_print(v);
	puts("-----------\n");

	expr = parse_sexpr(v);
	puts("\n + parsed expression");
	sexpr_describe(expr);

	eval(gs, expr);

	vector_free(v);
	puts(" ================== ================= ================= ");
	/* gc_debug_memory(); */
    }

    /* memory should be freed using GC
     * but for the moment it is not! */
}
#endif
