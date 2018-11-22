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
#include "chars.h"

#ifndef CONS
#  define CONS(sexpr) cons((sexpr), sexpr_nil())
#endif

/**
 * @brief static array of predefined Scheme keywords
 */
static keyword_t kwd[] = {
    {"begin", eval_begin},
    {"quote", eval_quote},
    {"eval", eval_eval},
    {"define", eval_define},
    {"undef", eval_undef},
    {"set", eval_set},
    {"setq", eval_setq},
    {"let", eval_let},
    {"let*", eval_let_asterisk},
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
 * s-expression or it does has an operator:
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
 * @see sexpr.h
 * @see scope.h
 *
 * @note this function may call itself recursively
 */
sexpr_t *eval_sexpr(scope_t * scope, sexpr_t * expr) {
    if (expr == NULL)
	return sexpr_err();

    sexpr_t *result = NULL, *op = NULL;	/* operator */
    bond_t *b = NULL;
    k_func kwd_func = eval_keyword(car(expr));

    /* ==================== ==================== ==================== */

#if DEBUG_EVALUATOR == DEBUG_ON
    puts("================ eval start ================");
    sexpr_print(expr), putchar('\n');
    puts("============================================");
#endif

    if (kwd_func)		/* symbol was a keyword */
	result = kwd_func(scope, cdr(expr));
    else if ((b = resolve_bond(scope, expr)))
	result = b->sexpr;	/* symbol was bounded  */
    else if (isatom(expr))
	result = expr;		/* just an atom/nil */

    /* ==================== ==================== ==================== */

#if DEBUG_EVALUATOR == DEBUG_ON
    puts(result ? "we have a result" : "there is no result");
    sexpr_print(result), putchar('\n');
#endif

    if (result)
	goto RET;		/* we have a result */

    op = eval_sexpr(scope, car(expr));

    err_raise(ERR_OP_NOT_FOUND, !islambda(op));

    if (err_log())
	goto FAILED;		/* no operator was found */

#if DEBUG_EVALUATOR == DEBUG_ON
    puts(op ? "we have an operator " : "there is no operator");
    sexpr_print(op), putchar('\n');
    puts("============== collecting args ==============");
#endif

    /* ==================== ==================== ==================== */

    sexpr_t *args = NULL, *tail = NULL;
    sexpr_t *foo = expr, *arg = NULL, *nil = sexpr_nil();

    /* creating a list of arguments */
    while (!isnil(foo = cdr(foo))) {
	arg = cons(eval_sexpr(scope, car(foo)), nil);

	err_raise(ERR_ERR, iserror(arg));

	if (err_log())
	    goto FAILED;

	if (!args)
	    args = arg;
	else
	    set_cdr(tail, arg);

	tail = arg;
    }

#if DEBUG_EVALUATOR == DEBUG_ON
    puts("============================================");
    printf("args: ");
    sexpr_print(args), putchar('\n');
    printf("length: %d \n", sexpr_length(args));
    puts("=============================================");
#endif

    /* ==================== ==================== ==================== */

    if (op->l->isnative)	/* call the native function */
	result = op->l->native->func(args);
    else {			/* evaluate the lambda's body */
	/* sexpr_print(args), */
	/*     sexpr_print(op->l->args), putchar('\n') ; */

	err_raise(ERR_LMBD_ARGS,
		  sexpr_length(args) != sexpr_length(op->l->args));

	if (err_log())
	    goto FAILED;

	scope_t *child = scope_init(scope);

	gc_setmark_sexpr(args, true);

	bind_lambda_args(child, op->l, args);
	result = eval_sexpr(child, op->l->body);

	gc_setmark_sexpr(result, true);
    }

  RET:

    err_raise(ERR_RSLT_NULL, !result);
    err_raise(ERR_ERR, iserror(result));

    if (err_log())
	goto FAILED;

#if DEBUG_EVALUATOR == DEBUG_ON
    puts("final result: ");
    sexpr_print(result), putchar('\n');
#endif

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
#if DEBUG_EVALUATOR == DEBUG_ON
	/* sexpr_print(vector_get(sexprs, i)); */
#endif
	tmp = eval_sexpr(gs, vector_get(sexprs, i));
	tmp = vector_push(v, tmp);

#if DEBUG_EVALUATOR == DEBUG_ON
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
 * @note `define` defines a symbol `(define symbol expr)`
 */
sexpr_t *eval_define(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, sexpr_length(expr) != 2);
    err_raise(ERR_ARG_TYPE, !issymbol(car(expr)));

    if (err_log())
	return sexpr_err();

    sexpr_t *evaled = NULL, *symbol = car(expr);
    bond_t bond = { symbol->s, NULL, false };

    /* TODO: if symbol is already exists, set it instead
     * we cannot call set since reserved raise an error */
    if (vector_find(scope->bonds, &bond) != NULL)
	return eval_set(scope, expr);


    evaled = eval_sexpr(scope, cadr(expr));

    err_raise(ERR_RSLT_NULL, !evaled);
    err_raise(ERR_ERR, iserror(evaled));

    if (err_log())
	return sexpr_err();

    scope_push_bond(scope, bond_new(symbol->s, evaled));

#if DEBUG_EVALUATOR == DEBUG_ON
    sexpr_print(evaled), putchar('\n');
    scope_describe(scope);
#endif

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
 * @param scope a scope (see notes)
 * @param expr the expression to evaluate
 *
 * @return a s-expression contains a lambda
 *
 * @see sexpr.h
 * @note `lambdas` are defined as `(lambda (args) (body))`
 * @note the `scope` is not used but since lambda is a keyword
 * so the function signature must contain a scope.
 */
sexpr_t *eval_lambda(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, !sexpr_length(expr));

    if (err_log())
	return sexpr_err();

    if (scope) {
	/* suppress warning */
    }

    return lambda_new(car(expr), cadr(expr));
}

sexpr_t *eval_setq_or_set(scope_t * scope, sexpr_t * expr, bool quoted) {
    err_raise(ERR_ARG_COUNT, sexpr_length(expr) != 2);
    err_raise(ERR_ARG_TYPE, !issymbol(car(expr)));

    if (err_log())
	return sexpr_err();

    bond_t *bond = resolve_bond(scope, car(expr));

    if (bond == NULL)
	return eval_define(scope, expr);
    else {
	setglobal(bond->sexpr, false);
	bond->sexpr = quoted ? cadr(expr) : eval_sexpr(scope, cadr(expr));
	setglobal(bond->sexpr, true);
    }

    return bond->sexpr;
}

sexpr_t *eval_set(scope_t * scope, sexpr_t * expr) {
    return eval_setq_or_set(scope, expr, false);
}

sexpr_t *eval_setq(scope_t * scope, sexpr_t * expr) {
    return eval_setq_or_set(scope, expr, true);
}

/*
 * : dereference the bond instead of setting the s-expr to nil
 *
 * since setting it to nil makes x resolves to nil not x
 */
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
 * FIXME: recreate this function in pure lisp
 */
sexpr_t *eval_eval(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, sexpr_length(expr) != 1);

    if (err_log())
	return sexpr_err();

    return eval_sexpr(scope, eval_sexpr(scope, car(expr)));
}

/**
 * @brief define temporary symbols to be used in the next s-expression(s)
 *
 * the `let` operator let us define temporary symbol which is really handy
 * when writing lambdas or other s-expressions.
 *
 * for example `(let ((x foo) (y bar)) body)` with Lisp magic is equivalent
 * to `((lambda (x y) body) foo bar)`. this function does the same by
 * evaluating that lambda (called `let-lambda` in here).
 *
 * @param scope a scope
 * @param expr the expression to evaluate
 *
 * @return a s-expression evaluation of a let s-expression
 *
 * @see lambda.h
 * @note let is defined as: `(let [label] ((arg param) ...) body)`
 * @note is label is not specified, let-lambda is used instead and can
 * be used to call the let-lambda recursively same as if label was
 * specified
 */
sexpr_t *eval_let(scope_t * scope, sexpr_t * expr) {
    err_raise(ERR_ARG_COUNT, sexpr_length(expr) < 1);
    err_raise(ERR_ARG_TYPE, !islist(car(expr)) && !issymbol(car(expr)));

    if (err_log())
	return sexpr_err();

    bool labeled = issymbol(car(expr));

    sexpr_t *args = NULL, *atail = NULL;
    sexpr_t *params = NULL, *ptail = NULL;

    sexpr_t *tmp = labeled ? cadr(expr) : car(expr),
	*symbol = labeled ? car(expr) : sexpr_symbol("let-lambda"),
	*body = labeled ? caddr(expr) : cadr(expr),
	*let = cons(symbol, sexpr_nil());

    while (!isnil(tmp)) {
	if (isnil(car(tmp))) {
	    args = sexpr_nil(), params = sexpr_nil();
	    break;
	}

	/* getting (arg param) */
	err_raise(ERR_ARG_TYPE, !islist(car(tmp)));
	err_raise(ERR_ARG_TYPE, !issymbol(caar(tmp)));

	if (err_log())
	    return sexpr_err();

	/* FIXME: create that fucking sexpr_append() */
	/* create that straming application */
	sexpr_t *arg = CONS(caar(tmp)), *param = CONS(cadar(tmp));

	/* appending the params to the whole let-lambda
	 * so that it would look like: (let-lambda param ...) */

	if (!params)
	    params = param;
	else
	    set_cdr(ptail, param);

	/* collecting let-lambda args
	 * which are the arg in (let ((arg param) ...) body) */
	if (!args)
	    args = arg;
	else
	    set_cdr(atail, arg);

	atail = args, ptail = params, tmp = cdr(tmp);
    }

    scope_t *child = scope_init(scope);
    scope_push_bond(child, bond_new(symbol->s, lambda_new(args, body)));
    set_cdr(let, params);

    return eval_sexpr(child, let);
}

/**
 * @brief define temporary symbols related to be used in the next
 * s-expression(s)
 *
 * the `let*` operator is like normal let but we can define symbols that
 * call each other except of the root symbol.
 *
 * for example `(let* ((x foo) (y (symbol? x))) body)` and again with
 * some help of Lisp magic is equivalent to something close to normal
 * let `(let ((x foo)) (let ((y symbol? x)) body))`.
 *
 * the main idea behind `let*` is to call let for each bonded symbol
 *
 * @param scope a scope
 * @param expr the expression to evaluate
 *
 * @return a s-expression evaluation of a let s-expression
 *
 * @see lambda.h
 * @see eval_let()
 * @note let* is defined as: `(let* [label] ((arg param) ...) body)`
 * @note bindings cannot refer to other binding in upper levels.
 * e.g. `(let* ((x y) (y 10)) body)` is not correct
 *
 * @bug this is not working as expected
 */
sexpr_t *eval_let_asterisk(scope_t * scope, sexpr_t * expr) {

    /* FIXME: find a way to create let for each argument
     *
     * the idea is to collect evaluated versions
     *
     * (let* ((x a) (b a) (c b)) body)
     *		      V
     * (let ((x a)) (let* ((b a) (c b)) body))
     *		      V
     * (let ((x a)) (let ((b a)) (let ((c b)) body)))
     *		      V
     * (let ((x a)) (let ((b a)) (let ((c b)) body)))).
     */

    /* puts("//////////"), sexpr_print(expr), puts(""); */

    err_raise(ERR_ARG_COUNT, sexpr_length(expr) < 1);
    err_raise(ERR_ARG_TYPE, (!islist(car(expr))
			     && !issymbol(car(expr))));

    if (err_log())
	return sexpr_err();

    bool labeled = issymbol(car(expr));
    sexpr_t *bindings = labeled ? cadr(expr) : car(expr),
	*symbol = labeled ? car(expr) : sexpr_symbol("let-lambda");

    /* sexpr_print(bindings), puts("0"); */
    /* sexpr_print(car(bindings)), puts("1"); */
    /* sexpr_print(cdr(bindings)), puts("2"); */
    /* sexpr_print(cadr(bindings)), puts("3"); */

    /*
     * getting bindings
     */
    err_raise(ERR_ARG_TYPE, !islist(car(bindings)));

    if (err_log())
	return sexpr_err();

    sexpr_t *let_asterisk = labeled
	? CONS(cons(symbol, CONS(car(bindings)))) :
	CONS(CONS(car(bindings))), *body =
	CONS(labeled ? caddr(expr) : cadr(expr));

    /* sexpr_print(let_asterisk), puts("4"); */
    /* sexpr_print(body), puts("5"); */

    if (!cadr(bindings)) {
	/* set the body as cdr */
	set_cdr(let_asterisk, body);
    } else {
	sexpr_t *foo = cons(cdr(bindings), body);
	/* sexpr_print(foo), puts("foo"); */
	sexpr_t *evaled = eval_let_asterisk(scope, foo);

	/* sexpr_print(evaled), puts("evaled"); */
	set_cdr(let_asterisk, cons(evaled, body));
    }

    /* sexpr_print(let_asterisk), puts("6"); */

    return eval_let(scope, let_asterisk);
}

sexpr_t *eval_begin(scope_t * scope, sexpr_t * expr) {
    sexpr_t *evaled = NULL, *tmp = expr;

    while (!isnil(tmp)) {
	evaled = eval_sexpr(scope, car(tmp));

	err_raise(ERR_RSLT_NULL, !evaled);
	err_raise(ERR_ERR, iserror(evaled));

	if (err_log())
	    return sexpr_err();

	tmp = cdr(tmp);
    }

    return evaled;
}

#undef CONS
