/*
 * this would split a text input into tokens, so that the parser
 * could create a parse-tree
 */
#include "../include/lexer.h"
#include "../include/vector.h"

static char getnc(const string_t str) {
    static string_t oldstr = NULL;
    static int i = 0;
    char c = EOF;

    if (!str || !str[i]) {
	i = 0;
	oldstr = NULL;
	return EOF;
    } else if (oldstr != str) {
	i = 0;
	oldstr = str;
    }

    c = str[i++];

    return c;
}

token_t *token_new(token_type type, token_string str, int depth) {
    token_t *token = malloc(sizeof *token);

    token->type = type;
    strcpy(token->buffer, str);
    token->depth = depth;

    return token;
}

/*!
 * this function reads a string, i.e. source code, and then returns
 * a vector of found tokens. in case of error, it would return NULL
 */
vector_t *read_tokens(const string_t code) {
#if defined LEXER_DEBUG
    assert(code);
#endif

    char c;
    vector_t *tokens = NULL;

  read:
    while (true) {
	/* ignore all whitespaces */
	do {
	    c = getnc(code);
	} while (c != EOF && !isspace(c));

	if (c == EOF) {
	    /* end of source code */
	    goto RET;
	}

	/* ignore all comments comments */
	if (c == ';') {
	    do {
		c = getnc(code);
	    } while (c != '\n' && c != '\r' && c != EOF);
	} else {
	    break;
	}
    }

#if defined LEXER_DEBUG
    assert(c != EOF);		/* this would always remain true */
#endif

    switch (c) {
    case '(':
	break;
    case ')':
	break;
    case '\'':
	break;
    case '\"':
	break;
    }

  RET:
    return tokens;
}

string_t stream_as_string(FILE * stream) {
    return "";
}

vector_t *read_stream_tokens(FILE * stream) {
    return read_tokens(stream_as_string(stream));
}

#if defined LEXER_DEBUG

void print_token(object_t t) {
    assert(t);

    token_t *foo = (token_t *) t;
    puts("depth \t token type \t value");
    printf("%d \t %d \t %s\n", foo->depth, foo->type, foo->buffer);
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
	tokens = read_tokens(exprs[i]);
	vector_log(tokens, print_token);
	vector_free(tokens);
    }
}
#endif
