/**
 * @file lexer.c
 *
 * @brief contains declarations of functionalities to verify
 * the grammar of a Scheme-like code and get all tokens in there
 *
 * read_tokens() and read_stream_tokens() are the top level functions
 * in this process. matter of fact, read_stream_tokens() calls
 * read_tokens() it collects after calling file_as_string()
 * to get the actual code.
 *
 * next_token() is used as a stepping function over the possible
 * tokens in the provided code. if no token is found or or it didn't
 * match the Scheme grammar, it returns an TOK_ERR type of token
 *
 * after the token is found, read_tokens() collects it into a Vector
 * and the those tokens would be used by the parser to convert that
 * set of tokens into a s-expression
 *
 * @see lexer.h about further documentation for each function
 * @see token.h information about Tokens
 * @see vector.h for information about Vectors
 * @see characters.h for information about getting strings
 *
 */

#include "lexer.h"

#include "characters.h"
#include "vector.h"

/**
 * this function reads a `code` string, i.e. source code
 *
 * @param code a string containing Scheme-like syntax
 *
 * @return a Vector of tokens
 */
vector_t *read_tokens(const string_t src) {
    vector_t *tokens = vector_new(token_free, token_print, NULL);
    token_t *token = NULL;
    int depth = 0;
    bool islastloop = false;
    string_t code = src;

    while ((token = next_token(code))) {
	switch (token->type) {
	case TOK_ERR:		/* error while lexing */
	    err_raise(ERR_TOK_ERR, true);
	    break;

	case EOL:		/* end of lexing */
	    err_raise(ERR_PRNS_BLNC, depth != 0);
	    islastloop = true;
	    break;

	case TOK_L_PAREN:	/* left paren */
	    token->depth = depth++;
	    break;

	case TOK_R_PAREN:	/* right paren */
	    token->depth = --depth;
	    err_raise(ERR_PRNS_CLS, depth < 0);
	    islastloop = depth ? false : true;
	    break;

	default:
	    token->depth = depth;
	    break;
	};

	if (err_log())
	    goto FAILED;
	else
	    vector_push(tokens, token);

	if (islastloop)
	    break;
    }

    return vector_compact(tokens);

  FAILED:

    return vector_free(tokens), NULL;
}

/**
 * this function reads a characters from @p stream, convert them
 * into a string and call read_tokens()
 *
 * @param code a string containing Scheme-like syntax
 *
 * @return a Vector of tokens
 */
vector_t *read_stream_tokens(const char *filename) {
    vector_t *vv = vector_new(vector_free, vector_print, NULL);
    string_t tmp = file_as_string(filename);

    while (getnc(tmp) != EOF)
	if (!clean_whitespaces(tmp) || !clean_comments(tmp))
	    break;
	else
	    vector_push(vv, read_tokens(tmp));

    free(tmp);

    return vector_compact(vv);
}

/**
 * get the next possible token from the @p code string
 *
 * @param code a string containing Scheme-like syntax
 *
 * @return a Token
 */
token_t *next_token(const string_t code) {
    token_type type;
    string_t vbuffer = NULL;
    bool accept_null = false;

    if (!clean_whitespaces(code) || !clean_comments(code)) {
	type = EOL;
	goto RET;
    }

    switch (type = predict_token_type(code)) {
    case TOK_QUOTE:
    case TOK_L_PAREN:
    case TOK_R_PAREN:
	accept_null = true;
	break;

    case TOK_STRING:
	vbuffer = read_as_string(code);
	break;

    case TOK_SYMBOL:
	vbuffer = read_as_symbol(code);
	break;

    case TOK_NUMBER:
	vbuffer = read_as_number(code);
	break;

    default:
    case EOL:
    case TOK_ERR:
	return NULL;
    };

    if (!vbuffer && !accept_null) {
	puts("i'm really lost");
	type = TOK_ERR;
    }

  RET:

    /* depth is initalized outside */
    return token_new(type, vbuffer, 0);
}

/**
 * takes any possible white-spaces from the @p code string
 *
 * @param code a string containing Scheme-like syntax
 *
 * @return false if we reach the EOF
 */
bool clean_whitespaces(string_t code) {
    char c;

    while ((c = getnc(code)) != EOF && isspace(c));

    if (c != EOF)
	ungetnc();

    return c != EOF;
}

/**
 * takes any possible comments from the @p code string
 *
 * @param code a string containing Scheme-like syntax
 *
 * @return false if we reach the EOF
 */
bool clean_comments(string_t code) {
    char c;

    while ((c = getnc(code)) == ';')
	do
	    c = getnc(code);
	while (c != '\n' && c != '\r' && c != EOF);

    if (c != EOF)
	ungetnc();

    return c != EOF;
}

/*
 * ==================================================================
 * the following read functions return NULL if an error occurs
 * ==================================================================
 */
string_t read_as_string(const string_t code) {
    string_t vbuffer = malloc(TOK_SIZE_LIMIT * sizeof(char));
    int i = 0;
    char c;

    while ((c = getnc(code)) != '\"' && c != EOF) {
	err_raise(ERR_SIZE_ERR, TOK_LIMIT(i));
	err_raise(ERR_EOF_ERR, c == EOF);

	if (err_log())
	    goto FAILED;

	vbuffer[i++] = c;
    }

    if (c != EOF && c != '\"')
	putchar(ungetnc());

    vbuffer[i] = '\0';

    return reduce_string_size(vbuffer);

  FAILED:

    return free(vbuffer), NULL;
}

string_t read_as_number(const string_t code) {
    string_t vbuffer = malloc(TOK_SIZE_LIMIT * sizeof(char));
    int i = 0;
    bool period_found = false;
    char c;

    while ((c = getnc(code)) != ' ' && c != ')') {
	/* FIXME: this is better be parsed as symbol instead */
	err_raise(ERR_NUM_DIG, !isdigit(c) && !strchr("+-.", c));
	err_raise(ERR_NUM_SIGN, strchr("+-", c) && i);
	err_raise(ERR_NUM_PRD, c == '.' && period_found);
	err_raise(ERR_SIZE_ERR, TOK_LIMIT(i));
	err_raise(ERR_EOF_ERR, c == EOF);

	if (err_log())
	    goto FAILED;

	period_found = (c == '.');
	vbuffer[i++] = c;
    }

    if (c == ')')
	ungetnc();

    vbuffer[i] = '\0';

    return reduce_string_size(vbuffer);

  FAILED:

    return free(vbuffer), NULL;
}

string_t read_as_symbol(const string_t code) {
    string_t vbuffer = malloc(TOK_SIZE_LIMIT * sizeof(char));
    string_t not_allowed = "()\'\"\\";
    char c;
    int i = 0;

    while ((c = getnc(code)) != ' ' && c != ')') {
	err_raise(ERR_SYM_ERR, strchr(not_allowed, c));
	err_raise(ERR_SIZE_ERR, TOK_LIMIT(i));
	err_raise(ERR_EOF_ERR, c == EOF);

	if (err_log())
	    goto FAILED;

	vbuffer[i++] = c;
    }

    if (c == ')')
	ungetnc();

    vbuffer[i] = '\0';

    return reduce_string_size(vbuffer);

  FAILED:

    return free(vbuffer), NULL;
}
