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

    vector_t *tokens = vector_new(token_free, token_print);
    token_t *token = NULL;
    int depth = 0;

    /* a loop over the whole source code */
    while ((token = next_token(code))) {
	/**
	** changing the depth of sexpr hepful at the parsing stage
	***********************************************************/
	switch (token->type) {
	case TOK_L_PAREN:
	    ++depth;
	    break;
	case TOK_R_PAREN:
	    --depth;
	    break;
	case TOK_EOF:
	    depth = -1;
	    puts("8888");
	    break;
	case TOK_ERR:
	    puts("TOK_ERR");
	    goto FAILED;
	default:
	    break;
	};

	token->depth = depth;
	vector_push(tokens, token);

	if (depth == -1) {
	    break;}
    }

    return tokens;

  FAILED:
    puts("$$$$");
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

    token_type type = TOK_EOF;
    string_t vbuffer = NULL;
    char c;

    if(!clean_whitespaces(code) || !clean_comments(code)) {
	/* end of file here */
	type = TOK_EOF;
	goto RET;
    }

    /* handling lists and literal strings */
    switch ((c = getnc(code))) {
    case '(':			/* beginning of a list */
	type = TOK_L_PAREN;
	goto RET;
    case ')':			/* end of a list */
	type = TOK_R_PAREN;
	goto RET;
    case '\'':			/* quoted list */
	type = TOK_S_QUOTE;
	goto RET;
    case '\"':			/* literal string */
	puts("this must executed one time");

	vbuffer = read_as_string(code);
	type = vbuffer ? TOK_STRING : TOK_ERR;

	goto RET;
    default:
	break;
    };

    printf("\n>>> %c\n", c);

    if (isdigit(c) || strchr(".-+", c)) {
	vbuffer = read_as_number(code); /* is it a number? */
	type = vbuffer ? TOK_NUMBER: TOK_ERR;
	goto RET;
    } else {
	vbuffer = read_as_atom(code); /* or an atom */
	type = vbuffer ? TOK_ATOM : TOK_ERR;
	goto RET;
    }

  RET:
    return token_new(type, vbuffer, 0);
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

    ungetnc(code);

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

    ungetnc(code);

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

    if (c == EOF) {
	noerror = 0;
	putchar(ungetnc(code));
	/* 0001 verify this line */
	goto FAILED;
    }

    vbuffer[i] = '\0';

    puts(vbuffer);

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
    while ((c = getnc(code)) != ' ') {
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
    while ((c = getnc(code)) != ' ') {
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

    vbuffer[i] = '\0';

    return reduce_string_size(vbuffer);

  FAILED:
    raise_error(stderr, error[noerror]);
    free(vbuffer);
    return NULL;
}

#if defined LEXER_DEBUG
void lexer_testing(void) {
    FILE *stream = stdout;

    char *exprs[] = {
	"(\"this is a string\")",
	"(4512 2054)",
	"\'(foo bar)",
	"    ; this is cool\n(bar baz)"
    }, c;

    int i, size = sizeof(exprs) / sizeof(exprs[0]);

    for (i = 0; i < size; ++i) {
	fprintf(stream, "expression: {%s}\n", exprs[i]);
	fputs("expression using getnc():", stream);
	while ((c = getnc(exprs[i])) != EOF) {
	    fputc(c, stream);
	}
	fputs("\n\n", stream);
    }

    vector_t *tokens;

    for (i = 0; i < size; ++i) {
	printf("\n-------------\n%s\n-------------\n", exprs[i]);
	tokens = read_tokens(exprs[i]);
	vector_print(tokens);
	vector_free(tokens);
	puts("%");
    }
}
#endif
