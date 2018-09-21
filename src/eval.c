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
    if (expr == NULL)
	return sexpr_nil();

    sexpr_t *result = NULL, *operator = NULL;
    k_func kwd_func = iskeyword(car(expr));

    /* ==================== ==================== ==================== */
#if EVALUATOR_DEBUG == DBG_ON
    puts("================ eval start ================");
    sexpr_print(expr);
    puts("============================================");
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
    sexpr_print(result);
#endif

    if (result)			/* we have a result */
	goto RET;
    else if (!(operator = eval_sexpr(scope, car(expr))))
	goto FAILED;		/* no operator was found */

#if EVALUATOR_DEBUG == DBG_ON
  puts(operator ? "we have an operator ":"there is no operator");
    sexpr_print(operator);
#endif

    /* ==================== ==================== ==================== */

    sexpr_t *args = NULL, *tail = NULL;
    sexpr_t *foo = expr, *bar = NULL;
    sexpr_t *nil = sexpr_nil();
    
    while (!isnil(foo = cdr(foo))) {
	bar = cons(eval_sexpr(scope, car(foo)), nil);
	/* puts("current arg"); */
	/* sexpr_print(tmp); */
	if (!args)
	    args = bar;
	else
	    set_cdr(tail, bar);
	/* puts("current args"); */
	/* sexpr_print(args); */
	tail = bar;
    }

#if EVALUATOR_DEBUG == DBG_ON
    puts("args: ");
    printf("length: %d \n", sexpr_length(args));
    sexpr_print(args);
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
    sexpr_print(result);
#endif

  RET:
    return result;

  FAILED:
    return NULL;
}

vector_t *eval_sexprs(scope_t * s, vector_t * sexprs) {
    int i;
    vector_t *v = vector_new(NULL, sexpr_print, NULL);

    for (i = 0; i < sexprs->size; ++i) {
#if EVALUATOR_DEBUG == DBG_ON
	puts(" ========== sexpr to eval =========== ");
	sexpr_print(vector_get(sexprs, i));
	puts(" ========== ================ =========== ");
	sexpr_t *tmp =
#endif
	    vector_push(v, eval_sexpr(s, vector_get(sexprs, i)));

#if EVALUATOR_DEBUG == DBG_ON
	printf("parsed sexpr: ");
	sexpr_print(tmp);
#endif

    }

    return vector_compact(v);
}

/* (define symbol 's-expr) */
sexpr_t *eval_define(scope_t * s, sexpr_t * expr) {
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
    vector_t *v = NULL, *w = NULL, *x = NULL;
    scope_t *gs = get_global_scope();

    /* scope_describe(gs); */

    v = read_stream_tokens("examples/arithmetic.scm");

    /* puts("stream of tokens"); */
    /* vector_print(v); */
    /* puts("-----------\n"); */

    w = parse_sexprs(v);
    x = eval_sexprs(gs, w);

    puts("======================================");
    for (int i = 0; i < w->size; ++i) {
	puts("================= // =================");
	sexpr_print(vector_get(w, i));
	sexpr_print(vector_get(x, i));
	puts("================= // =================");
    }
    puts("======================================");

    vector_free(w);
    vector_free(x);
    vector_free(v);

    gc_collect(true);
}
