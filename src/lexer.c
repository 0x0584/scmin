/**
 * @file lexer.c
 *
 * @brief this file contains declarations of funtionalities to verify
 * the grammar of a Scheme-like code and get all tokens in there
 *
 * read_tokens() and read_stream_tokens() are the top level functions
 * in this process. matter of fact, read_stream_tokens() calls
 * read_tokens() it collects after calling stream_as_string()
 * to get the actual code.
 *
 * next_token() is used as a stepping function over the possible
 * tokens in the provided code. if no token is found or or it didn't
 * match the Scheme grammar, it returns an TOK_ERR type of token
 *
 * TODO:
 *	    block the lexing process if an error occurs
 *
 * after the token is found, read_tokens() collects it into a Vector
 * and the those tokens would be used by the parser to convert thats
 * et of tokens into a s-expression
 *
 * @see @file lexer.h about further documentation for each function
 * @see @file token.h information about Tokens
 * @see @file vector.h for information about Vectors
 * @see @file characters.h for information about getting strings
 *
 * TODO:
 *	  stop the process somehow!
 */

#include "../include/main.h"
#include "../include/vector.h"

#include "../include/lexer.h"
#include "../include/characters.h"

/**
 * this function reads a @p code string, i.e. source code
 *
 * @param code a string containing Scheme-like syntax
 *
 * @return a Vector of tokens
 */
vector_t *read_tokens(const string_t code) {
#if defined LEXER_DEBUG
    assert(code != NULL);
#endif

    string_t error[] = {
	"ERROR: PARENS ARE NOT BALANCED",
	"ERROR: STARTING WITH A CLOSING PAREN",
	"ERROR: TOKEN IS NOT CORRECT"
    };
    vector_t *tokens = vector_new(token_free, token_print);
    token_t *token = NULL;
    int depth = 0, noerror = -1;

    bool_t islast = false;

    /***********************************************************
     * bug: getting characters is not good
     * todo: use a foor loop to get characters somehow.
     ***********************************************************/
    int i = 0;
    /* a loop over the whole source code */

    while ((token = next_token(code))) {
	printf("\n+++++++++++++++++\ntimes: %d\n", ++i);

	/* assert(token->type == TOK_ERR); */
	/* assert(token == NULL	 && token->type != TOK_S_QUOTE */
	/*	  && token->type != TOK_L_PAREN */
	/*	  && token->type != TOK_R_PAREN); */

	printf(" - -> %d\n", token->type);
	switch (token->type) {
	case TOK_ERR:		/* error while lexing */
	    noerror = 2;
	    goto FAILED;

	case TOK_EOL:		/* end of lexing */
	    if (depth != 0) {
		noerror = 0;
		goto FAILED;
	    }
	    islast = true;
	    break;

	case TOK_L_PAREN:	/* left paren */
	    token->depth = depth++;
	    break;

	case TOK_R_PAREN:	/* right paren */
	    token->depth = --depth;
	    puts("this is in");
	    if (depth < 0) {
		noerror = 1;
		goto FAILED;
	    }

	    islast = depth ? false : true;
	    break;

	case TOK_S_QUOTE:
	case TOK_D_QUOTE:
	default:
	    token->depth = depth;
	    break;
	};

	token_print(token);
	printf("islast:%s depth = %d\n", islast? "true":"false", depth);

	vector_push(tokens, token);
	vector_debug(stderr, tokens);

	if (islast) {
	    break;
	}

	puts("++++++ end ++++++");
	getchar();
    }

    return tokens;

  FAILED:

    raise_error(stderr, error[noerror]);

#if LEXER_DEBUG == DBG_ON
    assert(noerror != -1);
#endif

    vector_free(tokens);

    return NULL;
}

/**
 * this function reads a characters from @p stream, convert them
 * into a string and call read_tokens()
 *
 * @param code a string containing Scheme-like syntax
 *
 * @return a Vector of tokens
 */
vector_t *read_stream_tokens(FILE * stream) {
    return read_tokens(stream_as_string(stream));
}

/**
 * get the next possible token from the @p code string
 *
 * @param code a string containing Scheme-like syntax
 *
 * @return a Token
 */
token_t *next_token(const string_t code) {
#if defined LEXER_DEBUG
    assert(code != NULL);
#endif

    token_type type;
    string_t vbuffer = NULL;
    bool_t accept_null = false;
    token_t *token;

    if (!clean_whitespaces(code) || !clean_comments(code)) {
	type = TOK_EOL;
	goto RET;
    }

    switch (type = predict_token_type(getnc(code))) {
    case TOK_S_QUOTE:
    case TOK_L_PAREN:
    case TOK_R_PAREN:
	accept_null = true;
	printf(">>> %d, %c\n", type, accept_null ? 't' : 'f');
	break;

    case TOK_D_QUOTE:
	vbuffer = read_as_string(code);
	break;

    case TOK_LAMBDA:
	vbuffer = read_as_lambda(code);
	break;

    case TOK_ATOM:
	vbuffer = read_as_atom(code);
	break;

    case TOK_NUMBER:
	vbuffer = read_as_number(code);
	break;

    case TOK_EOL:
    case TOK_ERR:
    default:
	return NULL;
    };

#if LEXER_DEBUG == DBG_ON

#endif

    if (!vbuffer && !accept_null) {
	puts("i'm really lost");
	type = TOK_ERR;
    }

    /* printf("	 ==> %c\n", getnc(code)); */
    /* ungetnc(); */

  RET:
    token = token_new(type, vbuffer, 0);
    return token;
}

/**
 * takes any possible whitespaces from the @p code string
 *
 * @param code a string containing Scheme-like syntax
 *
 * @return false if we reach the EOF
 */
bool_t clean_whitespaces(string_t code) {
    char c;

    while ((c = getnc(code)) != EOF && isspace(c));

    if (c != EOF) {
	ungetnc();
    }

    return (c != EOF);
}

/**
 * takes any possible comments from the @p code string
 *
 * @param code a string containing Scheme-like syntax
 *
 * @return false if we reach the EOF
 */
bool_t clean_comments(string_t code) {
    char c;

    while ((c = getnc(code)) == ';') {
	do {
	    c = getnc(code);
	} while (c != '\n' && c != '\r' && c != EOF);
    }

    if (c != EOF) {
	ungetnc();
    }

    return (c != EOF);
}

/*
 * ==================================================================
 * the follwing read functions return NULL if an error occurs
 * TODO: tope the whole process then. this might need to implement
 * an error system
 * ==================================================================
 */
string_t read_as_string(const string_t code) {
    string_t vbuffer = malloc(TOK_SIZE_LIMIT * sizeof(char));
    string_t error[] = {
	"REACH EOF",
	"STRING IS TOO LONG OR HAS NO END"
    };
    int i = 0, noerror = 0;
    char c;

    /* this loop must end by its condition
     * otherwise this would count as an error */
    while ((c = getnc(code)) != '\"' && c != EOF) {
	vbuffer[i++] = c;
	/* the string is too long */
	if (i > TOK_SIZE_LIMIT - 1) {
	    noerror = 1;
	    goto FAILED;
	}
    }

    if (c != EOF && c != '\"') {
	putchar(ungetnc());
    }

    vbuffer[i] = '\0';

    printf(" => %s \n", vbuffer);

    return reduce_string_size(vbuffer);

  FAILED:
    raise_error(stderr, error[noerror]);
    free(vbuffer);

    return NULL;
}

string_t read_as_number(const string_t code) {
    string_t vbuffer = malloc(TOK_SIZE_LIMIT * sizeof(char));
    string_t error[] = {
	"NUMBER FORMAT ERROR: MULTIPLE SIGNS",
	"NUMBER FORMAT ERROR: MULTIPLE PERIODS",
	"NUMBER FORMAT ERROR: NOT A DIGIT ",
	"NUMBER IS TOO LONG"
    };
    int i = 0, noerror = 0;
    bool_t period_found = false;
    char c;

    /* this loop must end by its condition
     * otherwise this is an error */
    while ((c = getnc(code)) != ' ' && c != ')') {
	if ((c == '+' || c == '-')) {
	    if (i != 0) {
		noerror = 0;	/* something like +56- */
		goto FAILED;
	    }
	} else if (c == '.') {
	    if (!period_found) {
		period_found = true;
	    } else {
		noerror = 1;	/* something like 52.3.2 */
		goto FAILED;
	    }
	} else if (!isdigit(c)) {
	    noerror = 2;	/* something like 34abc */
	    goto FAILED;
	}

	vbuffer[i++] = c;

	/* the number is too long */
	if (i > TOK_SIZE_LIMIT - 1) {
	    noerror = 3;	/* surpassing the limit */
	    goto FAILED;
	}
    }

    if (c == ')') {
	ungetnc();
    }

    vbuffer[i] = '\0';

    return reduce_string_size(vbuffer);

    /* TODO: parse it as a number with warning */
  FAILED:
    raise_error(stderr, error[noerror]);
    free(vbuffer);
    return NULL;
}

string_t read_as_atom(const string_t code) {
    string_t vbuffer = malloc(TOK_SIZE_LIMIT * sizeof(char));
    string_t not_allowed = "()#\'\"";
    string_t error[] = {
	"ATOM IS TOO LONG OR HAS NO END",
	"ATOM CONTAINS CHARACTERS THAT ARE NOT ALLOWED"
    };
    char c;
    int i = 0, noerror = 0;

    /* this loop must end, same as before, by its condition
     * otherwise, this counts as an error*/
    while ((c = getnc(code)) != ' ' && c != ')') {
	if (strchr(not_allowed, c)) {
	    noerror = 1;	/* not allowed characters */
	    goto FAILED;
	}

	vbuffer[i++] = c;

	/* the string is too long */
	if (i > TOK_SIZE_LIMIT - 1) {
	    noerror = 0;	/* surpassing the limit */
	    goto FAILED;
	}
    }

    if (c == ')') {
	ungetnc();
    }

    vbuffer[i] = '\0';

    return reduce_string_size(vbuffer);

  FAILED:
    raise_error(stderr, error[noerror]);
    free(vbuffer);
    return NULL;
}

string_t read_as_lambda(const string_t code) {
    return code;
}

#if defined LEXER_DEBUG
void lexer_testing(void) {
    FILE *stream = stdout;

    char *exprs[] = {
	"(\"this is a string\")	 ",
	"(4512 2054)",
	"\'(foo bar)",
	"    ; this is cool\n(bar baz)"
    }, c;

    int i, size = sizeof(exprs) / sizeof(exprs[0]);

    for (i = 0; i < size; ++i) {
	fprintf(stream, "expression: {\n%s\n}\n", exprs[i]);
	fputs("expression using getnc():\n", stream);
	while ((c = getnc(exprs[i])) != EOF) {
	    fputc(c, stream);
	}
	fputs("\n\n", stream);
    }

    vector_t *tokens;

    for (i = 0; i < size; ++i) {
	printf("\n-------------\n%s\n-------------\n", exprs[i]);
	puts("get the tokens");
	tokens = read_tokens(exprs[i]);

	puts("printing the tokens");
	vector_debug(stdout, tokens);

	puts("free the tokens");
	vector_free(tokens);
	puts("%");
    }
}
#endif
