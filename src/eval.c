/**
 * @file eval.c
 *
 * @brief this file contains declarations of the core evaluation methods
 * that would take a parsed s-expression and evaluate it into
 * another s-expression.
 *
 * the main method here is eval_sexpr(), which takes a parsed s-expression
 * and and it's containing scope and return the result.
 *
 * @see @file eval.h
 * @see @file scope.h
 * @see @file pair.h
 * @see @file native.h
 */

#include "../include/eval.h"
#include "../include/scope.h"
#include "../include/native.h"

#include "../include/pair.h"
#include "../include/vector.h"
#include "../include/characters.h"

/**
 * pairing each keyword with its corespondent function.
 */
static struct {
    string_t keyword;
    k_func func;
} kwd[] = {
    {"quote", eval_quote},
    {"define", eval_define},
    {"if", eval_if},
    {NULL, NULL}
};

k_func iskeyword(sexpr_t * expr) {
    int i;

    if (!expr || !issymbol(expr))
	return NULL;

    for (i = 0; kwd[i].keyword; ++i)
	if (!strcmp(expr->s, kwd[i].keyword))
	    return kwd[i].func;

    return NULL;
}

/**
 * @brief evaluate a @p expr within a given @p scope and return the
 * evaluated s-expression, this function may call it self recursively
 * in order to evaluate inner s-expressions.
 *
 * before evaluating each expression we need to determine it's type, there
 * are native/predefined expression that would be executed directly using
 * a defined function, and other expressions that need evaluation.
 *
 * the first thing to do is to determine the type whether it's a normal
 * s-expression or does it has an operator:
 *
 *   + the expression'd be a keyword, if so, we pass the cdr(), i.e. args
 *     to the related function returned by iskeyword() so that it runs
 *     evaluation on it's own and returns an evaluated s-expression.
 *
 *   + the expression'd be bonded to a symbol, if so, resolve the bond
 *     using resolve_bond() and return the result.
 *
 *   + the expression'd be an atom, if so just return it. (if symbol was
 *     not bonded it would be returned laterally)
 *
 * if none of the above situation was true, then it's must has an operator,
 * so get the operator which is the car() of the expression, and then we
 * collect an evaluated version of the args by calling eval_sexpr() on each
 * cadr() until the cdr() of the expression is nil.
 *
 * next, we look to see if the operator was a native one, then we call the
 * related native function passing the arguments
 *
 *	ELSE
 *	    create a new child-scope;
 *	    bind lambda's args to the child-scope;
 *	    WHILE !isnil(expr->l->body) DO
 *		evaluate each s-expr using child-scope;
 *		return last expression's value;
 *
 * @param scope the contaning scope
 * @param expr a s-expreesion to evaluate
 *
 * @return the evaluated s-expression
 *
 * @note this fucntion may call itself recursively
 */
sexpr_t *eval_sexpr(scope_t * scope, sexpr_t * expr) {
    sexpr_t *result = NULL, *operator = NULL;
    k_func kwd_func = iskeyword(car(expr));

    /* ==================== ==================== ==================== */
#if EVALUATOR_DEBUG == DBG_ON
    puts("================ eval start ================");
    sexpr_describe(expr);
#endif

    if (kwd_func)		/* symbol was a keyword */
	result = kwd_func(scope, cdr(expr));
    else if (isbonded(scope, expr))	/* symbol was bounded  */
	result = resolve_bond(scope, expr);
    else if (isatom(expr))	/* just an atom/nil */
	result = expr;

    /* ==================== ==================== ==================== */

#if EVALUATOR_DEBUG == DBG_ON
    puts(result ? "we have a result" : "there is no result");
    sexpr_describe(result);
#endif

    if (result)			/* we have a result */
	goto RET;
    else if (!(operator = eval_sexpr(scope, car(expr))))
	goto FAILED;		/* no operator was found */

#if EVALUATOR_DEBUG == DBG_ON
  puts(operator ? "we have an operator ":"there is no operator");
    sexpr_describe(operator);
#endif

    /* ==================== ==================== ==================== */

    sexpr_t *args = NULL, *nil = sexpr_new(T_NIL);
    for (sexpr_t * tmp = expr, *tail; ispair(tmp = cdr(tmp)); tail = args)
	if (!args)
	    args = cons(eval_sexpr(scope, car(tmp)), nil);
	else
	    set_cdr(tail, cons(eval_sexpr(scope, car(tmp)), nil));

#if EVALUATOR_DEBUG == DBG_ON
    puts("args: ");
    sexpr_describe(args);
#endif

    /* ==================== ==================== ==================== */

    if (operator-> l->isnative)	/* call the native function */
	result = operator-> l->native->func(args);
    else {			/* interprete the lambda */
	sexpr_t *tmp = operator-> l->body;
	scope_t *child = scope_init(scope);

	/* TODO: bind lambda args to evaluated args */
	err_raise(ERR_LMBD_ARGS,
		    !bind_lambda_args(child, operator-> l, args));

	if (err_log())
	    goto FAILED;

	while (!isnil(tmp)) {
	    result = eval_sexpr(child, car(tmp));
	    tmp = cdr(tmp);
	}
    }

    err_raise(ERR_RSLT_NULL, !result);

    /* FIXME: after finishing the evaluation you need to clean the memory */
    /* gc_collect(false); */

#if EVALUATOR_DEBUG == DBG_ON
    puts("result: ");
    sexpr_describe(result);
#endif

  RET:
    return result;

  FAILED:
    return NULL;
}

/* (define symbol 's-expr) */
sexpr_t *eval_define(scope_t * s, sexpr_t * expr) {
    puts("================ evaluated define ================");
    sexpr_t *tmp = eval_sexpr(s, car(cdr(expr)));

#if EVALUATOR_DEBUG == DBG_ON
    puts("evaluated define");
    sexpr_describe(tmp);
#endif

    vector_push(s->bonds, bond_new(car(expr)->s, tmp));
    return tmp;
}

/* (if (condition) (true) (false)) */
sexpr_t *eval_if(scope_t * s, sexpr_t * expr) {
    if (s) {

    }

    return expr;

}

sexpr_t *eval_quote(scope_t * s, sexpr_t * expr) {
    if (s) {
	/* just to supress compiler warnings */
    }

    return car(expr);
}

#include "../include/lexer.h"
#include "../include/parser.h"

void eval_testing() {
    string_t exprs[] = {
	/* "(+ 11 (* 22 33))", */
	"(quote (a b c))",
	"'(a b c)",
	/* "(* 2 (+ 3 (* 6 2)))", */
	/* "(* (+ 5 5) (+ 3 (* (- 4 1) 2)))", */
	"(define x 4)",
	"(define y (+ 5 4))",
	"(+ x y)",
	"(define z '(+ 5 4))"
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
	printf("length of expression %d\n", sexpr_length(expr));

	eval_expr = eval_sexpr(gs, expr);

	puts("\n\n + evaluated expression");
	sexpr_describe(eval_expr);

	printf("%s", "\n + evaluated result: ");
	sexpr_print(eval_expr);
	printf("length of expression %d\n", sexpr_length(eval_expr));

	/* scope_describe(gs); */

	puts("\n===========================\n");

	vector_free(v);
	/* gc_collect(true); */
    }
}
