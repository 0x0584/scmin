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

/**
 * @brief create an evaluation stack such that we only keep the result
 * and discard the rest while collecting the garbage.
 *
 * for example the expression (+ 1 (- 5 4 5 (* 7 8) (/ 8 4)) 10), we get
 * the result of each expression and the lowest depth, in the example
 * above, it would be (/ 8 4) and (* 7 8) at depth 0, followed by
 * (- 5 4 5 X Y) where X and Y are the results of (* 7 8) and (/ 8 4)
 * respectively. and finally (+ 1 Z 10) where Z is the result of
 * (- 5 4 5 X Y).
 *
 * now, the expression (+ 1 (- 5 4 5 (* 7 8) (/ 8 4)) 10) is the parent
 * of (- 5 4 5 (* 7 8) (/ 8 4)) which is also the parent of both (* 7 8)
 * (/ 8 4) so i have to get something like this:
 *
 * push:eval_stack (- 5 (* 7 8) (/ 8 4))			  (0)
 * push:eval_stack (* 7 8)					  (1)
 *	 result = 56, pop:eval_stack push:(0):children_results result
 * push:eval_stack (/ 8 4)					  (1)
 *	 result = 2, pop:eval_stack push:(0):children_results result
 *	 result = -53 pop:eval_stack and since it's
 *	 scope->parent == NULL we do not push to a parent
 */
vector_t *eval_stack = NULL;

/**
 * @brief this is used to call nested car/cdr for many times
 *
 * for example, caaaadaddr would be interpreted in this
 * array so that, true is found where we need to call car,
 * and false if to call cdr.
 *
 * @see eval_keyword()
 * @see eval_nested_car_cdr()
 */
static bool call_cons_op[0x16];

/**
 * @brief the length of call_cons_op array
 */
static int call_cons_op_length = 0;

/**
 * @brief initialize a new context
 *
 * @param scope the scope of context
 * @param sexpr a s-expression
 *
 * @return a new context
 */
context_t *context_new(scope_t * scope, sexpr_t * sexpr) {
	context_t *context = gc_malloc(sizeof(context_t));

	context->scope = scope;
	context->sexpr = sexpr;
	context->result = NULL;
	context->locals = vector_new(NULL, sexpr_print, NULL);

	return context;
}

/**
 * @brief describes a the context `o`
 *
 * @param o a context to describe
 *
 * @note the parameter is an object so that we can use this
 * a printing function of any vector.
 * @see vector.h
 */
void context_describe(object_t o) {
	if (o == NULL)
		return;

	context_t *context = o;
	sexpr_t **stmp;
	int j;

	puts("\n------------");
	puts("\nthe sexpr: ");
	sexpr_print(context->sexpr);
	/* puts("\nthe scope: "); */
	/* scope_describe(context->scope); */
	puts("\nresult: ");
	sexpr_print(context->result), putchar('\n');
	puts("\nthe locals: ");

	for (j = 0; j < context->locals->size; ++j) {
		stmp = vector_get(context->locals, j);
		sexpr_print(*stmp), putchar('\n');
	}
	puts("------------");
}

/**
 * @brief frees the context `o`
 *
 * @note the parameter is an object so that we can use this
 * a printing function of any vector.
 */
void context_free(object_t o) {
	if (o == NULL)
		return;

	context_t *context = o;

	vector_free(context->locals);
	free(context);
}

/**
 * @brief get the last context in the evaluation stack
 *
 * @return a context
 */
context_t *last_context(void) {
	return !eval_stack->size ? NULL :
		vector_get(eval_stack, eval_stack->size - 1);
}

/**
 * @brief pin or unpin a context
 *
 * @param context the context to pin
 * @param pin whether to pin (`true`) or not (`false`)
 */
void setpin_context(context_t * context, bool pin) {
	if (context == NULL)
		return;

	sexpr_t **stmp;
	int j;

	gc_setpin_scope(context->scope, pin);
	gc_setpin_sexpr(context->result, pin);
	gc_setpin_sexpr(context->sexpr, pin);

	for (j = 0; j < context->locals->size; ++j) {
		stmp = vector_get(context->locals, j);
		gc_setpin_sexpr(*stmp, pin);
	}
}

/**
 * @brief push `context` to the evaluation stack
 *
 * @param context the context to push
 */
void context_push(context_t * context) {
	if (eval_stack == NULL)
		eval_stack = vector_new(context_free, context_describe, NULL);

	vector_push(eval_stack, context);
}

/**
 * @brief get the last context
 * @return popped a context as in a LIFO stack
 */
context_t *context_pop(void) {
	if (eval_stack == NULL)
		return NULL;

	context_t *item = vector_pop(eval_stack);
	setpin_context(item, false);

	/* context_describe(item); */

	return item;
}

/**
 * @brief static array of predefined Scheme keywords
 */
static keyword_t kwd[] = {
	{"0x0584", eval_nested_car_cdr},
	{"begin", eval_begin},
	{"cond", eval_cond},
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
 * @brief evaluate an expression `sexpr` within a given `scope`
 *
 * before evaluating each expression, we need to determine its type, there
 * are native/predefined expression that would be executed directly using
 * a predefined C function. and other expressions that are written in pure
 * Scheme/Lisp that need to be evaluated
 *
 * the first thing to do is to determine the type whether it's a normal
 * s-expression or it does has an operator:
 *
 *	 + if the expression is a keyword, we pass the cdr(), i.e. the args
 *	   to the related function returned by eval_keyword() so that it runs
 *	   evaluation on it's own and returns an evaluated s-expression.
 *
 *	 + if the expression is bonded to a symbol, resolve the bond using
 *	   resolve_bond() and return the result.
 *
 *	 + if the expression is an atom, we just return it. (if symbol is not
 *	   bonded it would be returned laterally)
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
 * @param sexpr a s-expression to evaluate
 *
 * @return the evaluated s-expression
 *
 * @see sexpr.h
 * @see scope.h
 *
 * @note this function may call itself recursively
 */
sexpr_t *eval_sexpr(scope_t * scope, sexpr_t * sexpr) {
	if (sexpr == NULL)
		return sexpr_err();

	sexpr_t *result = NULL, *op = NULL;	/* operator */
	bond_t *b = NULL;
	k_func kwd_func = eval_keyword(car(sexpr));

	/* create a new evaluation context for the current
	 * s-expression and its scope */
	context_t *context = context_new(scope, sexpr);
	context_push(context);

	vector_push(context->locals, &result);

	/* ==================== ==================== ==================== */

#if DEBUG_EVALUATOR == DEBUG_ON
	puts("================ eval start ================");
	sexpr_print(sexpr), putchar('\n');
	puts("============================================");
#endif

	if (kwd_func)		/* symbol was a keyword */
		result = kwd_func(scope, cdr(sexpr));
	else if ((b = resolve_bond(scope, sexpr)))
		result = b->sexpr;	/* symbol was bounded  */
	else if (isatom(sexpr))
		result = sexpr;		/* just an atom/nil */

	/* ==================== ==================== ==================== */

#if DEBUG_EVALUATOR == DEBUG_ON
	puts(result ? "we have a result" : "there is no result");
	sexpr_print(result), putchar('\n');
#endif

	if (result)
		goto RET;		/* we have a result */

	op = eval_sexpr(scope, car(sexpr));
	err_raise(ERR_OP_NOT_FOUND, !islambda(op));

	if (err_log())
		goto FAILED;		/* no operator was found */

	vector_push(context->locals, &op);

#if DEBUG_EVALUATOR == DEBUG_ON
	puts(op ? "we have an operator " : "there is no operator");
	sexpr_print(op), putchar('\n');
	puts("============== collecting args ==============");
#endif

	/* ==================== ==================== ==================== */

	sexpr_t *args = NULL, *nil = sexpr_nil();
	sexpr_t *foo = cdr(sexpr), *arg = NULL, *tail = NULL;

	vector_push(context->locals, &args);
	vector_push(context->locals, &nil);
	vector_push(context->locals, &foo);
	vector_push(context->locals, &arg);
	vector_push(context->locals, &tail);

	/* creating a list of arguments */
	while (!isnil(foo)) {
		arg = cons(eval_sexpr(scope, car(foo)), nil);

		err_raise(ERR_ERR, iserror(arg));

		if (err_log())
			goto FAILED;

		if (!args)
			args = arg;
		else
			set_cdr(tail, arg);

		tail = arg, foo = cdr(foo);
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
		err_raise(ERR_LMBD_ARGS,
				  sexpr_length(args) != sexpr_length(op->l->args));

		if (err_log())
			goto FAILED;

		scope_t *child = scope_init(scope);

		bind_lambda_args(child, op->l, args);
		result = eval_sexpr(child, op->l->body);
	}

  RET:

	err_raise(ERR_RSLT_NULL, !result);
	err_raise(ERR_ERR, iserror(result));

	if (err_log())
		goto FAILED;

	if (eval_stack != NULL) {
		context_t *ctx = context_pop();

		/* end of evaluation */
		if (eval_stack->size == 0) {
			vector_free(eval_stack);
			eval_stack = NULL;
		} else {
			last_context()->result = result;
			gc_collect(false);
		}

		context_free(ctx);
	}
#if DEBUG_EVALUATOR == DEBUG_ON
	printf("%s", "final result: "), sexpr_print(result), putchar('\n');
#endif

	return result;

  FAILED:

	vector_free(eval_stack);
	eval_stack = NULL;
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

	for (i = 0; i < sexprs->size; ++i)
		gc_setpin_sexpr(vector_get(sexprs, i), true);

	for (i = 0; i < sexprs->size; ++i) {
#if DEBUG_EVALUATOR == DEBUG_ON
		printf("eval: "), sexpr_print(vector_get(sexprs, i)),
			putchar('\n');
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
 * @brief determines whether a `sexpr` s-expression is a keyword or not
 *
 * @param sexpr s-expression
 *
 * @return `NULL` if the `sexpr` is not a keyword, or the keyword's
 * correspondent function otherwise
 */
k_func eval_keyword(sexpr_t * sexpr) {
	int i;

	if (!sexpr || !issymbol(sexpr))
		return NULL;		/* not a symbol */

	/* test nested cars and cdrs first */
	int length = strlen(sexpr->s) - 1;
	string_t str = sexpr->s;

	if (length < 2 || *str != 'c' || str[length] != 'r');
	else {
		call_cons_op_length = length - 1;
		bool is_cons_op = true;
		while (length - 1) {
			length--;
			if (str[length] == 'a')
				call_cons_op[call_cons_op_length - length] = true;
			else if (str[length] == 'd')
				call_cons_op[call_cons_op_length - length] = false;
			else {
				is_cons_op = false, call_cons_op_length = 0;
				break;
			}
		}

		if (is_cons_op)
			return kwd[0].func;
	}

	for (i = 0; kwd[i].keyword; ++i)
		/* looking for the keyword */
		if (!strcmp(sexpr->s, kwd[i].keyword))
			return kwd[i].func;

	return NULL;
}

/**
 * @brief returns the expression as it is
 *
 * quote gives the ability to just pass s-expression without
 * evaluating them, and since `sexpr` must be the cdr() of `'expr`,
 * we need to return the car() which is what we really want, and not
 * `sexpr` directly because we'll return the terminating nil as well.
 *
 * @param scope the containing scope
 * @param sexpr the expression to evaluate
 *
 * @return sexpr without evaluation
 * @note quote is defined as (quote expr)
 */
sexpr_t *eval_quote(scope_t * scope, sexpr_t * sexpr) {
	err_raise(ERR_ARG_COUNT, !sexpr_length(sexpr));

	if (err_log())
		return sexpr_err();

	if (scope || true)
		return car(sexpr);
}

/**
 * @brief define a symbol to hold a sexpr
 *
 * evaluates the cadr() `sexpr` and then creates a new bind
 * with the result and the symbol in the car() of `expr`
 *
 * @param scope the containing scope
 * @param sexpr the expression to evaluate
 *
 * @return the defined s-expression
 *
 * @see scope.h
 * @note `define` defines a symbol `(define symbol expr)`
 */

sexpr_t *eval_define(scope_t * scope, sexpr_t * sexpr) {
	err_raise(ERR_ARG_COUNT, sexpr_length(sexpr) != 2);
	err_raise(ERR_ARG_TYPE, !(issymbol(car(sexpr)) ^ islist(car(sexpr))));
	/* either (define foo ...) or (define (foo ..) ...) */
	err_raise(ERR_CANNOT_DEFINE, isreserved(scope, islist(car(sexpr))	\
											? caar(sexpr) : car(sexpr)));
	if (err_log())
		return sexpr_err();

	sexpr_t *symbol = islist(car(sexpr)) ? caar(sexpr) : car(sexpr),
		*evaled = NULL;
	/* bond_t bond = { symbol->s, NULL, false }; */

	/* TODO: if symbol is already exists, set it instead
	 * we cannot call set since reserved raise an error */
	/* if (vector_find(scope->bonds, &bond) != NULL) */
	/*		return eval_set(scope, sexpr); */

	if (islist(car(sexpr))) {
		/* prevent stuff like (define (bar 1 x t) ...) */
		sexpr_t *tmp = cdar(sexpr);

		while (!isnil(tmp)) {
			err_raise(ERR_ARG_TYPE, !(issymbol(car(tmp))));

			if (err_log())
				return sexpr_err();
			else
				tmp = cdr(tmp);
		}

		evaled = lambda_new(cdar(sexpr), cadr(sexpr));
	} else
		evaled = eval_sexpr(scope, cadr(sexpr));

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
 * @param sexpr the expression to evaluate
 *
 * @return the evaluate of expression that satisfies the condition
 *
 * @see sexpr.h
 * @note conditions are done as `(if (expr) (foo) (bar))`. `foo` is
 * evaluated when `sexpr` is not `nil`, otherwise evaluate `bar`
 */
sexpr_t *eval_if(scope_t * scope, sexpr_t * sexpr) {
	err_raise(ERR_ARG_COUNT, sexpr_length(sexpr) != 3);

	if (err_log())
		return sexpr_err();

	if (istrue(eval_sexpr(scope, car(sexpr))))
		return eval_sexpr(scope, cadr(sexpr));
	else
		return eval_sexpr(scope, caddr(sexpr));
}

/**
 * @brief creates lambda from `expr`
 *
 * initialize a non native lambda, car() are the args and cadr() is
 * the body
 *
 * @param scope a scope (see notes)
 * @param sexpr the expression to evaluate
 *
 * @return a s-expression contains a lambda
 *
 * @see sexpr.h
 * @note `lambdas` are defined as `(lambda (args) (body))`
 * @note the `scope` is not used but since lambda is a keyword
 * so the function signature must contain a scope.
 */
sexpr_t *eval_lambda(scope_t * scope, sexpr_t * sexpr) {
	err_raise(ERR_ARG_COUNT, !sexpr_length(sexpr));

	if (err_log())
		return sexpr_err();

	if (scope) {
		/* suppress warning */
	}

	return lambda_new(car(sexpr), cadr(sexpr));
}

sexpr_t *eval_setq_or_set(scope_t * scope, sexpr_t * sexpr, bool quoted) {
	err_raise(ERR_ARG_COUNT, sexpr_length(sexpr) != 2);
	err_raise(ERR_ARG_TYPE, !issymbol(car(sexpr)));

	if (err_log())
		return sexpr_err();

	bond_t *bond = resolve_bond(scope, car(sexpr));

	if (bond == NULL)
		return eval_define(scope, sexpr);
	else {
		setglobal(bond->sexpr, false);
		bond->sexpr =
			quoted ? cadr(sexpr) : eval_sexpr(scope, cadr(sexpr));
		setglobal(bond->sexpr, true);
	}

	return bond->sexpr;
}

sexpr_t *eval_set(scope_t * scope, sexpr_t * sexpr) {
	return eval_setq_or_set(scope, sexpr, false);
}

sexpr_t *eval_setq(scope_t * scope, sexpr_t * sexpr) {
	return eval_setq_or_set(scope, sexpr, true);
}

/*
 * : dereference the bond instead of setting the s-expr to nil
 *
 * since setting it to nil makes x resolves to nil not x
 */
sexpr_t *eval_undef(scope_t * scope, sexpr_t * sexpr) {
	err_raise(ERR_ARG_COUNT, sexpr_length(sexpr) != 1);
	err_raise(ERR_ARG_TYPE, !issymbol(car(sexpr)));

	if (err_log())
		return sexpr_err();

	bond_t *bond = resolve_bond(scope, car(sexpr));

	if (bond == NULL)
		return sexpr_nil();

	/* for the moment undef just sets the symbol to nil */
	setglobal(bond->sexpr, false);
	bond->sexpr = sexpr_symbol(bond->symbol);
	setglobal(bond->sexpr, true);

	return sexpr_true();
}

/*
 * FIXME: recreate this function in pure lisp
 */
sexpr_t *eval_eval(scope_t * scope, sexpr_t * sexpr) {
	err_raise(ERR_ARG_COUNT, sexpr_length(sexpr) != 1);

	if (err_log())
		return sexpr_err();

	return eval_sexpr(scope, eval_sexpr(scope, car(sexpr)));
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
 * @param sexpr the expression to evaluate
 *
 * @return a s-expression evaluation of a let s-expression
 *
 * @see lambda.h
 * @note let is defined as: `(let [label] ((arg param) ...) body)`
 * @note is label is not specified, let-lambda is used instead and can
 * be used to call the let-lambda recursively same as if label was
 * specified
 */
sexpr_t *eval_let(scope_t * scope, sexpr_t * sexpr) {
	err_raise(ERR_ARG_COUNT, sexpr_length(sexpr) < 1);
	err_raise(ERR_ARG_TYPE, (!islist(car(sexpr)) &&
							 !issymbol(car(sexpr))));
	if (err_log())
		return sexpr_err();

	bool labeled = issymbol(car(sexpr));

	sexpr_t *args = NULL, *atail = NULL;
	sexpr_t *params = NULL, *ptail = NULL;

	sexpr_t *arg_nil = sexpr_nil(), *param_nil = sexpr_nil();

	sexpr_t *bindings = labeled ? cadr(sexpr) : car(sexpr),
		*body = labeled ? caddr(sexpr) : cadr(sexpr);

	while (!isnil(bindings)) {
		/* getting each binding (arg param) */
		err_raise(ERR_ARG_TYPE, !islist(car(bindings)));
		err_raise(ERR_ARG_TYPE, !issymbol(caar(bindings)));

		if (err_log())
			return sexpr_err();

		/* FIXME: create that fucking sexpr_append() */
		/* create that straming application */
		sexpr_t *arg = cons(caar(bindings), arg_nil),
			*param = cons(cadar(bindings), param_nil);

		/* appending the params to the whole let-lambda
		 * so that it would look like: (let-lambda param ...) */
		if (!params)
			params = param;
		else
			set_cdr(ptail, param);

		/* collecting let-lambda args which are the
		 * arg in (let ((arg param) ...) body) */
		if (!args)
			args = arg;
		else
			set_cdr(atail, arg);

		atail = arg, ptail = param, bindings = cdr(bindings);
	}

	scope_t *child = scope_init(scope);
	sexpr_t *symbol = labeled ? car(sexpr) : sexpr_symbol("let-lambda"),
		*let = cons(symbol, sexpr_nil());
	sexpr_t *l = lambda_new(args, body);
	scope_push_bond(child, bond_new(symbol->s, l));
	set_cdr(let, params);

	/* sexpr_println(let); */
	/* sexpr_println(l); */

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
 * @param sexpr the expression to evaluate
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
sexpr_t *eval_let_asterisk(scope_t * scope, sexpr_t * sexpr) {

	/* FIXME: find a way to create let for each argument
	 *
	 * the idea is to collect evaluated versions
	 *
	 * (let* ((x a) (b a) (c b)) body)
	 *						  V
	 * (let ((x a)) (let* ((b a) (c b)) body))
	 *						  V
	 * (let ((x a)) (let ((b a)) (let* ((c b)) body)))
	 *						  V
	 * (let ((x a)) (let ((b a)) (let ((c b)) body))).
	 */

	puts("//////////"), sexpr_print(sexpr), puts("");

	err_raise(ERR_ARG_COUNT, sexpr_length(sexpr) < 1);
	err_raise(ERR_ARG_TYPE, (!islist(car(sexpr))
							 && !issymbol(car(sexpr))));

	if (err_log())
		return sexpr_err();

	bool labeled = issymbol(car(sexpr));
	sexpr_t *bindings = labeled ? cadr(sexpr) : car(sexpr),
		*symbol = labeled ? car(sexpr) : sexpr_symbol("let-lambda");

	sexpr_print(bindings), puts(":bindings");
	sexpr_print(car(bindings)), puts("car(bindings)");
	sexpr_print(cdr(bindings)), puts("cdr(bindings)");
	sexpr_print(cadr(bindings)), puts("cadr(bindings)");

	/*
	 * getting bindings
	 */
	err_raise(ERR_ARG_TYPE, !islist(car(bindings)));

	if (err_log())
		return sexpr_err();

	/* WTF IS THIS! I HAVE TO CLEAN THIS MESS */
	/* ((symbol (x y))) */
	sexpr_t *let_a = cons(symbol, sexpr_nil()),
		/* ((body)) */
		*body = cons(labeled ? caddr(sexpr) : cadr(sexpr), sexpr_nil());

	sexpr_print(let_a), puts("let_a");
	sexpr_print(body), puts("body");

	while (!isnil(bindings)) {
		sexpr_t *foo = cons(cdr(bindings), body);
		sexpr_print(foo), puts("foo");
		set_cdr(let_a, cons(foo, body));

		bindings = cdr(bindings);
	}

	set_cdr(let_a, body);	/* set the body as cdr */

	sexpr_print(let_a), puts("final let_a");

	return eval_let(scope, let_a);
}


sexpr_t *eval_nested_car_cdr(scope_t * scope, sexpr_t * sexpr) {
	err_raise(ERR_ARG_TYPE, !islist(sexpr));

	if (err_log())
		return sexpr_err();

	sexpr_t *tmp = eval_sexpr(scope, car(sexpr));
	int i;

	for (i = 0; i < call_cons_op_length; ++i) {
		tmp = call_cons_op[i] ? car(tmp) : cdr(tmp);

		err_raise(ERR_ARG_TYPE, tmp == NULL);

		if (err_log())
			return sexpr_err();
	}

	return tmp;
}

sexpr_t *eval_begin(scope_t * scope, sexpr_t * sexpr) {
	sexpr_t *evaled = sexpr_nil(), *tmp = sexpr;

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

sexpr_t *eval_cond(scope_t * scope, sexpr_t * sexpr) {
	sexpr_t *tmp = sexpr;

	while (!isnil(tmp)) {
		err_raise(ERR_ARG_COUNT, sexpr_length(car(tmp)) != 2);

		if (err_log())
			return sexpr_err();

		if ((issymbol(caar(tmp)) && strcmp(caar(tmp)->s, "else"))
			|| istrue(eval_sexpr(scope, caar(tmp))))
			return eval_sexpr(scope, cadar(tmp));

		tmp = cdr(tmp);
	}

	return sexpr_nil();
}
