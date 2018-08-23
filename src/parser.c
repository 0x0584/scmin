/*
 * this would take a set of tokens and then creates a parse-tree
 * PS: or something like that
 */

#include "../include/parser.h"

#include "../include/vector.h"
#include "../include/token.h"

#include "../include/pair.h"
#include "../include/sexpr.h"

sexpr_t *parse_sexpr(vector_t * tokens) {
    sexpr_t *expr = NULL;
    token_t *token = NULL;

    while (true) {
	token = vector_peek(tokens);
	if (!token)
	    break;

	switch (token->type) {
	case TOK_L_PAREN:
	    expr = parse_as_list(tokens);
	    break;

	case TOK_NUMBER:
	    expr = parse_as_number(token->vbuffer);
	    break;
	case TOK_ATOM:
	    expr = parse_as_atom(token->vbuffer);
	    break;
	case TOK_D_QUOTE:
	    expr = parse_as_string(token->vbuffer);
	    break;

	default:
	    break;
	}

	/* i guess that this is never reached */
	if (token->type == TOK_R_PAREN && token->depth == 0) {
	    puts("not reached");
	    break;		/* we have finished parsing */
	}
    }

    assert(expr != NULL);

    vector_free(tokens);	/* free the tokens in the parser does
				 * not seem like a good idea. or is it? */


    return expr;
}

/* there is no check parens here, the lexer did the job
 * FIXME: improve the code */
sexpr_t *parse_as_list(vector_t * tokens) {
    string_t error[] = {
	"LIST HAS NO CLOSE PAREN"
    };
    int noerror = 0;
    bool_t isfinished = false, isfirstloop = true;
    sexpr_t *expr;
    sexpr_t *head = NULL, *tail = NULL, *value;

    token_t *token = NULL;

    while (true) {
	token = vector_peek(tokens);

	if (token->type == TOK_R_PAREN) {
	    /* empty list check '() */
	    /* HERE */
	    if (isfirstloop) {
		expr = sexpr_new(T_NIL);
	    }

	    isfinished = true;
	    break;
	} else {
	    /* FIXME: handle 'a to (quote a) */
	    switch (token->type) {
	    case TOK_L_PAREN:
		value = parse_as_list(tokens);
		break;

	    case TOK_NUMBER:
		value = parse_as_number(token->vbuffer);
		break;
	    case TOK_D_QUOTE:
		value = parse_as_string(token->vbuffer);
		break;
	    case TOK_ATOM:
		value = parse_as_atom(token->vbuffer);
		break;

	    default:
		value = sexpr_new(T_NIL);
		break;
	    }
	}

	/* ====================== testing this ====================== */
	expr = cons(value, sexpr_new(T_NIL));

	if (!head) {
	    head = expr;
	} else {
	    set_cdr(tail, expr);
	}

	tail = expr;
	/* ========================================================== */

	token_free(token);
	isfirstloop = false;
    }

    if (!isfinished) {
	noerror = 0;
	goto FAILED;
    }

    assert(expr != NULL);

    return head;

  FAILED:

    raise_error(stderr, error[noerror]);
    return NULL;
}

sexpr_t *parse_as_number(string_t value) {
    assert(value != NULL);

    sexpr_t *expr = sexpr_new(T_NUMBER);

    expr->v.n = strtod(value, NULL);

    return expr;
}

sexpr_t *parse_as_string(string_t value) {
    sexpr_t *expr = sexpr_new(T_STRING);

    expr->v.s = strdup(value);

    return expr;
}

sexpr_t *parse_as_boolean(string_t value) {
    sexpr_t *expr = sexpr_new(T_BOOLEAN);

    if (!strcmp(value, "nil") || !strcmp(value, "#f")) {
	expr->v.b = false;
    } else if (!strcmp(value, "#t") || !strcmp(value, "t")) {
	expr->v.b = true;
    }

    return NULL;
}

/*
expr: 0x557dfac88740, type:4 (CONS-PAIR)
    content:
    expr: 0x557dfac886e0, type:3 (ATOM)
	content: +

 */
sexpr_t *parse_as_atom(string_t value) {
    sexpr_t *expr;

    if (!(expr = parse_as_boolean(value))) {
	expr = parse_as_string(value);
	expr->type = T_ATOM;
	assert(expr != NULL);
    }

    return expr;
}

#if PARSER_DEBUG == DBG_ON
#  include "../include/lexer.h"

void parser_testing(void) {
    string_t exprs[] = {
	"(+ 11111 (* 22222 33333))",
	"(\"this is a string\")	 ",
	"(car \'((foo bar) (fuzz buzz)))",
	"    ; this is cool\n(bar baz)"
    };

    int i, size = sizeof(exprs) / sizeof(exprs[0]);

    /*
    puts(" ================= s-exprs ================= ");

    sexpr_t *number = sexpr_new(T_NUMBER);
    number->v.n = 11;
    sexpr_describe(number);

    sexpr_t *str = sexpr_new(T_STRING);
    str->v.s = "this is";
    sexpr_describe(str);

    sexpr_t *atom = sexpr_new(T_ATOM);
    atom->v.s = "foo";
    sexpr_describe(atom);
    */
    /*
    puts("\n ================= list ================= ");
    puts("(11 \"this is\" foo)");
    sexpr_t *list = cons(number, cons(str, cons(atom, sexpr_new(T_NIL))));
    sexpr_describe(list);
    */
    /*
    puts("\n ================= complex list ================= ");

    sexpr_t *atom0 = sexpr_new(T_ATOM);
    atom0->v.s = "bar";
    sexpr_describe(atom0);

    sexpr_t *number0 = sexpr_new(T_NUMBER);
    number0->v.n = 3.14159;
    sexpr_describe(number0);

    puts("(3.14159 (11 \"this is\" foo) bar)");
    sexpr_t *list0 =
	cons(number0, cons(list, cons(atom0, sexpr_new(T_NIL))));
    sexpr_describe(list0);
    */
    /*
    puts("\n ================= cons operations ================= ");
    puts("(car list)");
    sexpr_describe(car(list));

    puts("(cdr list)");
    sexpr_describe(cdr(list));

    puts("(car (cdr list))");
    sexpr_describe(car(cdr(list)));

    puts("(car (cdr (cdr list)))");
    sexpr_describe(car(cdr(cdr(list))));

    puts("(car (cdr (cdr (cdr (list)))))");	// must return T_NIL //
    sexpr_describe(car(cdr(cdr(cdr(list)))));
    */
    /*
    puts("\n ================= more on lists ================= ");
    sexpr_t *fuzz = sexpr_new(T_NUMBER);
    fuzz->v.n = 465;

    sexpr_t *tripl_list = cons(
	cons(
	    cons(
		fuzz,
		sexpr_new(T_NIL)
		),
	    sexpr_new(T_NIL)
	    ),
	sexpr_new(T_NIL)
    );
    */
    /*
    puts("(((465)))");
    sexpr_describe(tripl_list);

    puts("\n(car '(((465)))) ; ((465))");
    sexpr_describe(car(tripl_list));

    puts("\n(car (car '(((465))))) ; (465)");
    sexpr_describe(car(car(tripl_list)));

    puts("\n(car (cdr '(((465))))) ; NIL");
    sexpr_describe(car(cdr(tripl_list)));
    */

    vector_t *v = NULL;
    /* this is just for testing */
    sexpr_t *atom_plus = sexpr_new(T_ATOM);
    atom_plus->v.s = "+";
    sexpr_t *number_1 = sexpr_new(T_NUMBER);
    number_1->v.n = 111;
    sexpr_t *atom_times = sexpr_new(T_ATOM);
    atom_times->v.s = "*";
    sexpr_t *number_2 = sexpr_new(T_NUMBER);
    number_2->v.n = 111;
    sexpr_t *number_3 = sexpr_new(T_NUMBER);
    number_3->v.n = 111;

    sexpr_t *expr = NULL, *expected = cons(atom_plus,
					   cons(number_1,
						cons(cons(atom_times,
							  cons(number_2,
							       cons
							       (number_3,
								sexpr_new
								(T_NIL)
							       )
							  )
						     ),
						     sexpr_new(T_NIL)
						)
					   )
	);

    for (i = 0; i < size; ++i) {
	v = read_tokens(exprs[i]);
	puts(exprs[i]);
	/* puts("\n--------- expected ----------"); */
	/* sexpr_describe(expected); */
	/* puts("\n-----------------------------"); */

	vector_print(v);
	puts("===================");
	expr = parse_sexpr(v);


	sexpr_describe(expr);
	vector_free(v);
    }

    /* memory should be freed using GC
     * but for the moment it is not! */
}
#endif
