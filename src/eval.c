#include "../include/eval.h"
#include "../include/scope.h"
#include "../include/context.h"
#include "../include/native.h"

#include "../include/pair.h"
#include "../include/vector.h"
#include "../include/characters.h"

typedef enum KEYWORD {
    K_NOT_KEYWORD = 0,

    K_DEFINE,
    K_IF,
    K_AND,
    K_OR,
    K_NOT
} keyword_t;

keyword_t iskeyword(sexpr_t * expr) {
    static string_t keyword[] = {
	"define",
	"if",
	"and",
	"or",
	"not"
    };
    static int i = 0, size = sizeof(keyword) / sizeof(keyword[0]);

    if (!issymbol(expr))
	return K_NOT_KEYWORD;

    for (i = 0; i < size; ++i) {
	if (!strcmp(expr->s, keyword[i]))
	    return (keyword_t) i + 1;
    }

    return K_NOT_KEYWORD;
}

sexpr_t *eval_keyword(keyword_t k, sexpr_t * expr);

/* FIXME: handle erros in a more sofisticated way */
sexpr_t *eval(scope_t * s, sexpr_t * expr) {
    sexpr_t *operator, *tail = NULL, *args, *tmp = NULL;
    sexpr_t *nil = sexpr_new(T_NIL);
    keyword_t key;

    tmp = car(expr);

    if ((key = iskeyword(tmp)))	/* evaluate the keyword */
	return eval_keyword(key, cdr(expr));
    else if (isbonded(s, tmp))	/* resolve symbol  */
	return resolve_bond(s, expr);
    else if (!ispair(tmp))	/* just an atom/nil */
	return expr;


    /* take the operator of the s-expression */
    if (!(operator = eval(s, car(expr))))
	return car(expr);

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
    sexpr_t *expr = NULL, *eval_expr = NULL;
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

	eval_expr = eval(gs, expr);

	vector_free(v);
	puts("========= =========");
	/* gc_debug_memory(); */
    }

    /* memory should be freed using GC
     * but for the moment it is not! */
}
#endif
