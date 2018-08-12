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

void token_free(void *t) {
    free(t);
}

token_t *next_token(char *code) {
    /* this would save a pointer to the last character
     * we stopped  at last time */
    char c;
    token_t *token = NULL;

    #if defined LEXER_DEBUG
    /* assert(save == NULL); */
    /* assert(code != NULL); */
    #endif

    /* ignoring whitespaces and comments */
    while (true) {
	/* ignore all whitespaces */
	do {
	    c = getnc(code);
	} while (c != EOF && isspace(c));

	if (c == EOF) {
	    goto RET;		/* end of source code */
	}

	/* ignore all comments */
	if (c == ';') {
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
	puts("list starting");
	break;
    case ')':			/* end of a list */
	puts("list ending");
	break;
    case '\'':			/* quoted list */
	puts("quoted list");
	break;
    case '\"':			/* literal string */
	puts("string starting/ending");
	break;
    default:
	break;
    };

    /* handling atoms */
    /* handling numbers */
    /* handling lambdas */

    token = token_new(TOK_ATOM, "foo", 1);

  RET:
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

    vector_t *tokens = vector_new();;
    token_t *token;

    puts(code);

    /* a loop over the whole source code */
    while ((token = next_token(code))) {
	vector_add(tokens, token);
    }

    return tokens;
}

string_t stream_as_string(FILE * stream) {
    fputc('\0', stream);
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
	vector_free(tokens, token_free);
    }
}
#endif
