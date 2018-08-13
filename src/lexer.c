/*
 * this would split a text input into tokens, so that the parser
 * could create a parse-tree
 */
#include "../include/lexer.h"
#include "../include/vector.h"

static char getnc(const string_t str) {
    static string_t oldstr = NULL;
    static int i = 0;

#if defined LEXER_DEBUG
    assert(str != NULL);
#endif

    if (!str || !str[i]) {
	i = 0;
	oldstr = NULL;
	return EOF;
    } else if (oldstr != str) {
	i = 0;
	oldstr = str;
    }

    return str[i++];
}

token_t *token_new(token_type type, string_t str, int depth) {
    token_t *token = malloc(sizeof *token);

    token->type = type;
    token->buffer = str;
    token->depth = depth;

    return token;
}

void token_free(object_t o) {
    assert(((token_t *) o)->buffer != NULL);

    if (((token_t *) o)->buffer != NULL)
	free(((token_t *) o)->buffer);
    free(o);
}

string_t reduce_string_size(string_t str) {
    return realloc(str, strlen(str) * sizeof(char));
}

string_t read_string(const string_t code) {
    string_t buffer = malloc(TOK_SIZE_LIMIT * sizeof(char));
    int i = 0;
    char c;

    /* this loop must end by its condition
     * otherwise this would count as an error */
    while ((c = getnc(code)) != '\"') {
	buffer[i++] = c;
	/* the string is too long */
	if (i > TOK_SIZE_LIMIT - 1) {
	    goto FAILED;
	}
    }

    buffer[i] = '\0';

    puts(buffer);

    return reduce_string_size(buffer);

  FAILED:
    raise_error(stderr, "STRING IS TOO LONG OR HAS NO END");
    free(buffer);
    return NULL;
}

string_t read_number(const string_t code) {
    string_t buffer = malloc(TOK_SIZE_LIMIT * sizeof(char));
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

	buffer[i++] = c;

	/* the number is too long */
	if (i > TOK_SIZE_LIMIT - 1) {
	    noerror = 3;	/* surpassing the limit */
	    goto FAILED;
	}
    }

    buffer[i] = '\0';

    return reduce_string_size(buffer);

  FAILED:
    raise_error(stderr, error[noerror]);
    free(buffer);
    return NULL;
}

string_t read_atom(const string_t code) {
    string_t buffer = malloc(TOK_SIZE_LIMIT * sizeof(char));
    string_t not_allowed = "()#\'\"{}[]";
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

	buffer[i++] = c;

	/* the string is too long */
	if (i > TOK_SIZE_LIMIT - 1) {
	    noerror = 0;	/* surpassing the limit */
	    goto FAILED;
	}
    }

    buffer[i] = '\0';

    return reduce_string_size(buffer);

  FAILED:
    raise_error(stderr, error[noerror]);
    free(buffer);
    return NULL;
}

token_t *next_token(const string_t code, int depth) {
    if (depth == -1){
	return NULL;}

    token_type type = TOK_ATOM;
    string_t buffer = NULL;
    char c;

#if defined LEXER_DEBUG
    /* assert(save == NULL); */
    /* assert(code != NULL); */
#endif

    /* ignoring white spaces and comments */
    while (true) {
	do {			/* ignore all white spaces */
	    c = getnc(code);
	} while (c != EOF && isspace(c));

	if (c == EOF) {		/* end of source code */
	    goto RET;
	}

	if (c == ';') {		/* ignore all comments */
	    do {
		c = getnc(code);
	    } while (c != '\n' && c != '\r' && c != EOF);
	} else {
	    break;
	}
    }

    /* handling lists and literal strings */
    switch (c) {
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
	if ((buffer = read_string(code))) {
	    type = TOK_STRING;
	    puts("sss");
	} else {
	    type = TOK_ERR;
	}
	goto RET;
    default:
	break;
    };

    /* is it a number? */
    if (isdigit(c) || strchr(".-+", c)) {
	if ((buffer = read_number(code))) {
	    type = TOK_NUMBER;
	} else {
	    type = TOK_ERR;
	}
	goto RET;
    } else {			/* or an atom */
	if ((buffer = read_atom(code))) {
	    type = TOK_ATOM;
	} else {
	    type = TOK_ERR;
	}
	goto RET;
    }

  RET:
    return token_new(type, buffer, depth);
}

/*!
 * this function reads a string, i.e. source code, and then returns
 * a vector of found tokens. in case of error, it would return NULL
 */
vector_t *read_tokens(const string_t code) {
#if defined LEXER_DEBUG
    assert(code);
#endif

    vector_t *tokens = vector_new();;
    token_t *token = NULL;
    int depth = 0;

    /* a loop over the whole source code */
    while ((token = next_token(code, depth))) {
	token_print(token);
	/* changing the depth of sexpr, hepful at the parsing stage */
	switch (token->type) {
	case TOK_L_PAREN:
	    ++depth;
	    break;
	case TOK_R_PAREN:
	    --depth;
	    break;
	case TOK_EOF:
	    depth = -1;
	default:
	case TOK_ERR:
	    goto FAILED;
	};

	vector_add(tokens, token);
    }

    return tokens;

  FAILED:
    puts("$$$$");
    vector_free(tokens, token_free);
    return NULL;
}

string_t stream_as_string(FILE * stream) {
    fputc('\0', stream);
    return "";
}

vector_t *read_stream_tokens(FILE * stream) {
    return read_tokens(stream_as_string(stream));
}

#if defined LEXER_DEBUG

void token_print(object_t t) {
    assert(t);

    token_t *foo = (token_t *) t;
    puts("depth \t type \t  sexpr");
    printf("%-8d %-8d %-8s\n", foo->depth, foo->type, foo->buffer);
}

void lexer_testing(void) {
    FILE *stream = stdout;

    char *exprs[] = {
	"(\"this is a string\")",
	"(4512 2054)",
	"\'(foo bar)"
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
	vector_log(tokens, token_print);
	vector_free(tokens, token_free);
	puts("%");
    }
}
#endif
