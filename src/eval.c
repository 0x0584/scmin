#include "../include/eval.h"
#include "../include/scope.h"
#include "../include/context.h"
#include "../include/native.h"

#include "../include/pair.h"
#include "../include/vector.h"
#include "../include/characters.h"

k_func iskeyword(sexpr_t * expr);
static struct {
    string_t keyword;
    k_func func;
} kwd[] = {
    {"define", eval_define},
    {"if", eval_if},
    {NULL, NULL}
};

sexpr_t *eval(scope_t * s, sexpr_t * expr) {
    sexpr_t *operator = NULL, *tail = NULL, *args = NULL;
    sexpr_t *tmp = NULL, *nil = sexpr_new(T_NIL);
    k_func kwd_func;

    kwd_func = iskeyword(tmp = expr);

    if (kwd_func != NULL)	/* evaluate the keyword */
	return kwd_func(s, cdr(expr));
    else if (isbonded(s, tmp)){	/* resolve symbol  */
	return resolve_bond(s, tmp);
    } else if (!ispair(tmp))	/* just an atom/nil */
	return expr;

    /* take the operator of the s-expression */
    if (!(operator = eval(s, car(expr))))
	return NULL;

    /* make a list of evaluated arguments */
    for (tmp = expr; ispair(tmp = cdr(tmp)); tail = args) {
	if (!args)
	    args = cons(eval(s, car(tmp)), nil);
	else
	    set_cdr(tail, cons(eval(s, car(tmp)), nil));
    }

    if (operator-> l->isnative)	/* call the native function */
	return operator-> l->native->func(args);
    else {			/* interprete the lambda */
	scope_t *child = scope_init(s);
	sexpr_t *result = NULL;

	/* TODO: bind lambda args to evaluated args */
	if (!bind_lambda_args(child, operator-> l, args)) {
	    raise_error(stdout, "cannot bind lambda args");
	    goto FAILED;
	}

	tmp = operator-> l->body;

	while (!isnil(tmp)) {
	    result = eval(child, car(tmp));
	    tmp = cdr(tmp);
	}

	return result;
    }

  FAILED:
    return NULL;
}

k_func iskeyword(sexpr_t * expr) {
    int i;

    if (!issymbol(expr))
	return NULL;

    for (i = 0; kwd[i].keyword; ++i)
	if (!strcmp(expr->s, kwd[i].keyword))
	    return kwd[i].func;

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
	"(* 2 (+ 3 (* 6 2)))"
	/* "	; this is cool\n(bar baz)", */
	/* "(\"this is a string\")	 " */
    };

    int i, size = sizeof(exprs) / sizeof(exprs[0]);
    vector_t *v = NULL;
    sexpr_t *expr = NULL, *eval_expr = NULL;
    scope_t *gs = global_scope_init();

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

	/* gc_collect(true); */
	vector_free(v);
    }
}
#endif
