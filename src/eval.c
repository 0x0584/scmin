#include "../include/eval.h"
#include "../include/scope.h"
#include "../include/context.h"
#include "../include/native.h"

#include "../include/pair.h"
#include "../include/vector.h"
#include "../include/characters.h"

static struct {
    string_t keyword;
    k_func func;
} kwd[] = {
    {"define", eval_define},
    {"if", eval_if},
    {NULL, NULL}
};

k_func iskeyword(sexpr_t * expr) {
    int i;

    if (!issymbol(expr))
	return NULL;

    for (i = 0; kwd[i].keyword; ++i)
	if (!strcmp(expr->s, kwd[i].keyword))
	    return kwd[i].func;

    return NULL;
}

sexpr_t *eval(scope_t * s, sexpr_t * expr) {
    sexpr_t *result = NULL, *operator = NULL;
    sexpr_t *args = NULL, *nil = sexpr_new(T_NIL);
    k_func kwd_func = iskeyword(expr);

    /* ==================== ==================== ==================== */

    if (!IS_NULL(kwd_func))	/* symbol was a keyword */
	result = kwd_func(s, cdr(expr));
    else if (isbonded(s, expr))	/* symbol was bounded  */
	result = resolve_bond(s, expr);
    else if (!ispair(expr))	/* just an atom/nil */
	result = expr;

    /* ==================== ==================== ==================== */

    if (!IS_NULL(result))	/* we have a result */
	goto RET;
    else if (!(operator = eval(s, car(expr))))
	goto FAILED;		/* no operator was found */

    /* ==================== ==================== ==================== */

    for (sexpr_t *tmp = expr, *tail; ispair(tmp = cdr(tmp)); tail = args)
	if (!args)
	    args = cons(eval(s, car(tmp)), nil);
	else
	    set_cdr(tail, cons(eval(s, car(tmp)), nil));

    /* ==================== ==================== ==================== */

    if (operator-> l->isnative)	/* call the native function */
	result = operator-> l->native->func(args);
    else {			/* interprete the lambda */
	sexpr_t *tmp = operator-> l->body;
	scope_t *child = scope_init(s);

	/* TODO: bind lambda args to evaluated args */
	if (!bind_lambda_args(child, operator-> l, args)) {
	    raise_error(stdout, "CANNOT BIND LAMBDA ARGS");
	    goto FAILED;
	}

	while (!isnil(tmp)) {
	    result = eval(child, car(tmp));
	    tmp = cdr(tmp);
	}
    }

    if (IS_NULL(result))
	raise_error(stdout, "FINAL RESULT SHOULD NOT BE NULL");

  RET:
    return result;

  FAILED:
    return NULL;
}

/* (define symbol 's-expr) */
sexpr_t *eval_define(scope_t * s, ...) {
    return NULL;
}

/* (if (condition) (true) (false)) */
sexpr_t *eval_if(scope_t * s, ...) {
    return NULL;
}

#if EVALUATOR_DEBUG == DBG_ON

#  include "../include/lexer.h"
#  include "../include/parser.h"

void eval_testing() {
    string_t exprs[] = {
	"(+ 11 (* 22 33))",
	/* "(quote (a b c))", */
	"(* 2 (+ 3 (* 6 2)))",
	"(* (+ 5 5) (+ 3 (* (- 4 1) 2)))",
	/* "	; this is cool\n(bar baz)", */
	/* "(\"this is a string\")	 " */
    };

    int i, size = sizeof(exprs) / sizeof(exprs[0]);
    vector_t *v = NULL;
    sexpr_t *expr = NULL, *eval_expr = NULL;
    scope_t *gs = get_global_scope();

    /* scope_describe(gs); */

    for (i = 0; i < size; ++i) {
	printf("\n + parsing %s\n", exprs[i]);

	v = read_tokens(exprs[i]);

	puts("\n + list of tokens");
	vector_print(v);
	puts("-----------\n");

	expr = parse_sexpr(v);

	puts("\n + parsed expression");
	sexpr_describe(expr);

	puts("\n + parsed result");
	sexpr_print(expr);

	eval_expr = eval(gs, expr);

	puts("\n\n + evaluated expression");
	sexpr_describe(eval_expr);

	printf("%s", "\n + evaluated result: ");
	sexpr_print(eval_expr);

	puts("\n\n===========================\n");

	vector_free(v);
	gc_collect(true);
    }
}
#endif
