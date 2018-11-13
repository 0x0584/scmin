/**
 * @file eval.c
 *
 * @brief declarations of the core evaluation functions, those would take
 * a parsed s-expressions and evaluate them into another s-expression.
 *
 * the main method here is eval_sexpr() for a single expression, and
 * eval_sexprs() for a vector of sexprs, both functions require parsed
 * s-expressions. in addition of the expressions to evaluate, a
 * containing scope is required, which would basically hold bonded
 * and/or predefined ones.
 *
 * @see eval.h
 * @see parser.h
 * @see scope.h
 * @see native.h
 */

#include "eval.h"
#include "scope.h"
#include "native.h"

#include "pair.h"
#include "vector.h"
#include "characters.h"

/**
 * @brief static array of predefined Scheme keywords
 */
static keyword_t kwd[] = {
    {"quote", eval_quote},
    {"eval", eval_eval},
    {"define", eval_define},
    {"undef", eval_undef},
    {"set", eval_set},
    {"setq", eval_setq},
    {"if", eval_if},
    {"lambda", eval_lambda},
    {NULL, NULL}
};

/**
 * @brief evaluate an expression `expr` within a given `scope`
 *
 * before evaluating each expression, we need to determine its type, there
 * are native/predefined expression that would be executed directly using
 * a predefined C function. and other expressions that are written in pure
 * Scheme/Lisp that need to be evaluated
 *
 * the first thing to do is to determine the type whether it's a normal
 * s-expression or does it has an operator:
 *
 *   + if the expression is a keyword, we pass the cdr(), i.e. the args
 *     to the related function returned by eval_keyword() so that it runs
 *     evaluation on it's own and returns an evaluated s-expression.
 *
 *   + if the expression is bonded to a symbol, resolve the bond using
 *     resolve_bond() and return the result.
 *
 *   + if the expression is an atom, we just return it. (if symbol is not
 *     bonded it would be returned laterally)
 *
 * if none of the above situation was true, then it's must has an operator;
 * so we get the operator (evaluating the car() of the expression) and then
 * we collect an evaluated version of the args by calling eval_sexpr() on
 * each cadr() until we reach the end i.e. a `nil` at the end.
 *
 * next, we look to see if the operator was a native one, if so; we call the
 * related native function passing the arguments. otherwise we create a new
 * scope (child scope of the current scope) then bind the lambda arguments
 * using bind_lambda_args() to the arguments in the child scope and evaluate
 * the lambda's body passing the new child scope. finally, the last result
 * is returned
 *
 * @param scope the containing scope
 * @param expr a s-expression to evaluate
 *
 * @return the evaluated s-expression
 *
 * @see #SYMBOLIC_EXPRESSION_TYPE
 * @see scope.c
 *
 * @note this function may call itself recursively
 */
sexpr_t *eval_sexpr(scope_t * scope, sexpr_t * expr) {
    if (expr == NULL)
	return sexpr_nil();

    sexpr_t *result = NULL, *op = NULL;	/* operator */
    bond_t *b = NULL;
    k_func kwd_func = eval_keyword(car(expr));

    /* ==================== ==================== ==================== */

#if EVALUATOR_DEBUG == DBG_ON
    puts("================ eval start ================");
    sexpr_print(expr);
    puts("============================================");
#endif

    if (kwd_func)		/* symbol was a keyword */
	result = kwd_func(scope, cdr(expr));
    else if ((b = resolve_bond(scope, expr)))
	result = b->sexpr;	/* symbol was bounded  */
    else if (isatom(expr))
	result = expr;		/* just an atom/nil */

    /* ==================== ==================== ==================== */

#if EVALUATOR_DEBUG == DBG_ON
    puts(result ? "we have a result" : "there is no result");
    sexpr_print(result);
#endif

    if (result)
	goto RET;		/* we have a result */

    err_raise(ERR_OP_NOT_FOUND, !resolve_bond(scope, car(expr)));

    if (err_log())
	goto FAILED;		/* no operator was found */

    op = eval_sexpr(scope, car(expr));

#if EVALUATOR_DEBUG == DBG_ON
    puts(op ? "we have an operator " : "there is no operator");
    sexpr_print(op);
#endif

    /* ==================== ==================== ==================== */

    sexpr_t *args = NULL, *tail = NULL;
    sexpr_t *foo = expr, *bar = NULL, *nil = sexpr_nil();

    /* creating a list of arguments */
    while (!isnil(foo = cdr(foo))) {
	bar = cons(eval_sexpr(scope, car(foo)), nil);

	if (!args)
	    args = bar;
	else
	    set_cdr(tail, bar);

	tail = bar;
    }

#if EVALUATOR_DEBUG == DBG_ON
    puts("args: ");
    printf("length: %d \n", sexpr_length(args));
    sexpr_print(args);
#endif

    /* ==================== ==================== ==================== */

    if (op->l->isnative)	/* call the native function */
	result = op->l->native->func(args);
    else {			/* evaluate the lambda's body */
	err_raise(ERR_LMBD_ARGS,
		  sexpr_length(args) != sexpr_length(op->l->args));

	if (err_log())
	    goto FAILED;

	scope_t *child = scope_init(scope);

	bind_lambda_args(child, op->l, args);
	result = eval_sexpr(child, op->l->body);
    }

    err_raise(ERR_RSLT_NULL, !result);

#if EVALUATOR_DEBUG == DBG_ON
    puts("result: ");
    sexpr_print(result);
#endif

  RET:
    return result;

  FAILED:
    return NULL;
}

/**
 * @brief just like eval_sexpr(), but with a vector of s-expressions
 *
 * @param sexprs a vector of s-expressions
 *
 * @return a vector of the evaluated s-expressions
 *
 * @see eval_sexpr()
 * @see vector.h
 */
vector_t *eval_sexprs(vector_t * sexprs) {
    vector_t *v = vector_new(NULL, sexpr_print, NULL);
    sexpr_t *tmp = NULL;
    scope_t *gs = get_global_scope();
    int i;

    for (i = 0; i < sexprs->size; ++i) {
#if EVALUATOR_DEBUG == DBG_ON
	sexpr_print(vector_get(sexprs, i));
#endif
	tmp = eval_sexpr(gs, vector_get(sexprs, i));
	tmp = vector_push(v, tmp);

#if EVALUATOR_DEBUG == DBG_ON
	printf(" > "), sexpr_print(tmp), putchar('\n');
#endif
    }

    return vector_compact(v);
}

/**
 * @brief determines whether a `expr` s-expression is a keyword or not
 *
 * @param expr s-expression
 *
 * @return `NULL` if the `expr` is not a keyword, or the keyword's
 * correspondent function otherwise
 */
k_func eval_keyword(sexpr_t * expr) {
    int i;

    if (!expr || !issymbol(expr))
	return NULL;		/* not a symbol */

    for (i = 0; kwd[i].keyword; ++i)
	/* looking for the keyword */
	if (!strcmp(expr->s, kwd[i].keyword))
	    return kwd[i].func;

    return NULL;
}

/**
 * @brief returns the expression as it is
 *
 * quote gives the ability to just pass s-expression without
 * evaluating them, and since `expr` must be the cdr() of `'expr`,
 * we need to return the car() which is what we really want, and not
 * `expr` directly because we'll return the terminating nil as well.
 *
 * @param scope the containing scope
 * @param expr the expression to evaluate
 *
 * @return expr without evaluation
 * @note quote is defined as (quote expr)
 */
sexpr_t *eval_quote(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, !sexpr_length(expr));

    if (err_log())
	return sexpr_err();

    if (scope || true)
	return car(expr);
}

/**
 * @brief define a symbol to hold a sexpr
 *
 * evaluates the cadr() `expr` and then creates a new bind
 * with the result and the symbol in the car() of `expr`
 *
 * @param scope the containing scope
 * @param expr the expression to evaluate
 *
 * @return the defined s-expression
 *
 * @see scope.h
 * @note `defines` are defined as (define symbol expr)
 */
sexpr_t *eval_define(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, sexpr_length(expr) != 2);
    err_raise(ERR_ARG_TYPE, !issymbol(car(expr)));

    if (err_log())
	return sexpr_err();

    sexpr_t *evaled = NULL, *symbol = car(expr);
    bond_t *bond = vector_find(scope->bonds, &(bond_t) {
			       symbol->s, NULL, false
			       }
    );

    if (bond != NULL)
	return eval_set(scope, expr);

    evaled = eval_sexpr(scope, cadr(expr));
    scope_push_bond(scope, bond_new(symbol->s, evaled));

    return symbol;
}

/**
 * @brief performers a conditional based on the car() of `expr`
 *
 * the condition is the car() of `expr`, if it was `true`, checked using
 * istrue(), then cadr() is evaluated, otherwise the caddr() if evaluated
 * instead.
 *
 * @param scope the containing scope
 * @param expr the expression to evaluate
 *
 * @return the evaluate of expression that satisfies the condition
 *
 * @see sexpr.h
 * @note conditions are done as `(if (expr) (foo) (bar))`. `foo` is
 * evaluated when `expr` is not `nil`, otherwise evaluate `bar`
 */
sexpr_t *eval_if(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, sexpr_length(expr) != 3);

    if (err_log())
	return sexpr_err();

    if (istrue(eval_sexpr(scope, car(expr))))
	return eval_sexpr(scope, cadr(expr));
    else
	return eval_sexpr(scope, caddr(expr));
}

/**
 * @brief creates lambda from `expr`
 *
 * initialize a non native lambda, car() are the args and cadr() is
 * the body
 *
 * @param scope the containing scope
 * @param expr the expression to evaluate
 *
 * @return a lambda s-expression
 *
 * @see sexpr.h
 * @note `lambdas` are defined as `(lambda (args) (body))`
 */
sexpr_t *eval_lambda(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, !sexpr_length(expr));

    if (err_log())
	return sexpr_err();

    return lambda_new(scope, car(expr), cadr(expr));
}

/*
 * TODO: remove code redundancy between set and setq
 */
sexpr_t *eval_set(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, sexpr_length(expr) != 2);
    err_raise(ERR_ARG_TYPE, !issymbol(car(expr)));

    if (err_log())
	return sexpr_err();

    bond_t *bond = resolve_bond(scope, car(expr));

    if (bond == NULL)
	return eval_define(scope, expr);
    else {
	setglobal(bond->sexpr, false);
	bond->sexpr = eval_sexpr(scope, cadr(expr));
	setglobal(bond->sexpr, true);
    }

    return bond->sexpr;
}

sexpr_t *eval_setq(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, sexpr_length(expr) != 2);
    err_raise(ERR_ARG_TYPE, !issymbol(car(expr)));

    if (err_log())
	return sexpr_err();

    bond_t *bond = resolve_bond(scope, car(expr));

    if (bond == NULL)
	return eval_define(scope, expr);

    setglobal(bond->sexpr, false);
    bond->sexpr = cadr(expr);
    setglobal(bond->sexpr, true);

    return bond->sexpr;
}

sexpr_t *eval_undef(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, sexpr_length(expr) != 1);
    err_raise(ERR_ARG_TYPE, !issymbol(car(expr)));

    if (err_log())
	return sexpr_err();

    bond_t *bond = resolve_bond(scope, car(expr));

    if (bond == NULL)
	return sexpr_nil();

    /* for the moment undef just sets the symbol to nil */
    setglobal(bond->sexpr, false);
    bond->sexpr = sexpr_nil();
    setglobal(bond->sexpr, true);

    return sexpr_true();
}

/*
 * TODO: recreate this function in pure lisp
 */
sexpr_t *eval_eval(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, sexpr_length(expr) != 1);

    if (err_log())
	return sexpr_err();

    return eval_sexpr(scope, eval_sexpr(scope, car(expr)));
}
