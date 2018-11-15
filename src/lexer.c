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
 * set of tokens into a #SYMBOLIC_EXPRESSION
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
 * @param src a string containing Scheme-like syntax
 *
 * @return a Vector of tokens
 */
vector_t *read_tokens(const string_t src) {
    vector_t *tokens = vector_new(token_free, token_print, NULL);
    token_t *token = NULL;
    bool islastloop = false, isfirstloop = true;
    string_t code = src;
    int depth = 0;

    /* reading the tokens in the code on after the other  */
    while ((token = next_token(code))) {
	if (isfirstloop && token->type != TOK_L_PAREN) {
	    token_free(token);
	    goto FAILED;
	}

	switch (token->type) {
	case TOK_ERR:		/* error while lexing */
	    err_raise(ERR_TOK_ERR, true);
	    break;

	case TOK_EOL:		/* end of lexing */
	    err_raise(ERR_PRNS_BLNC, depth != 0);
	    islastloop = true;
	    break;

	case TOK_L_PAREN:	/* left parenthesis */
	    token->depth = depth++;
	    break;

	case TOK_R_PAREN:	/* right parenthesis */
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

	vector_push(tokens, token), isfirstloop = false;

	if (islastloop)
	    break;
    }

    return vector_compact(tokens);

  FAILED:

    return vector_free(tokens), NULL;
}

/**
 * this function reads a characters from `stream`, convert them
 * into a string and call read_tokens()
 *
 * @param filename a string containing Scheme-like syntax
 *
 * @return a Vector of tokens
 */
vector_t *read_stream_tokens(const string_t filename) {
    vector_t *vv = vector_new(vector_free, vector_print, NULL);
    string_t tmp = file_as_string(filename);

    while (getnc(tmp) != EOF) {
	if (!clean_source_code(tmp))
	    break;

	vector_push(vv, read_tokens(tmp));
    }

    free(tmp);

    return vector_compact(vv);
}

/**
 * @brief move through all the tokens in a giving @p code.
 *
 * first, it calls clean_comments() and clean_whitespaces() to
 * clean the @p code. after that, using getnc() to keep track on
 * the stream, it gets a character. it calls predict_token_type()
 * to determine the type of the next token. the it calls one of the
 * read functions based on the result.
 *
 * @return a Token
 *
 * @see getnc()
 * @see token.h
 *
 * @note this function modifies the static values in of getnc()
 */
token_t *next_token(const string_t code) {
    token_type type;
    string_t vbuffer = NULL;
    bool accept_null = false;

    if (!clean_source_code(code)) {
	type = TOK_EOL;
	goto RET;
    }

    switch (type = predict_token_type(code)) {
    case TOK_QUOTE:
    case TOK_L_PAREN:
    case TOK_R_PAREN:
	accept_null = true;
	break;

    case TOK_STRING:
	vbuffer = read_string(code);
	break;

    case TOK_SYMBOL:
	vbuffer = read_symbol(code);
	break;

    case TOK_NUMBER:
	vbuffer = read_number(code);
	break;

    default:
    case TOK_EOL:
    case TOK_ERR:
	goto FAILED;
    };

    if (!vbuffer && !accept_null)
	goto FAILED;

  RET:

    /* depth is initialized outside */
    return token_new(type, vbuffer, 0x0584);

  FAILED:

    return NULL;
}

/*
 * ==================================================================
 * the following read functions return NULL if an error occurs
 * ==================================================================
 */

/**
 * reads the token value from `code` as string
 *
 * @param code a Scheme/Lisp syntax
 *
 * @return value of the token if type matches, or `NULL` otherwise
 *
 * @see getnc()
 * @see token.h
 *
 * @note this function modifies the static values in of getnc()
 */
string_t read_string(const string_t code) {
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
	ungetnc();

    vbuffer[i] = '\0';

    return reduce_string_size(vbuffer);

  FAILED:

    return free(vbuffer), NULL;
}

/**
 * reads the token value from `code` as number
 *
 * @param code a Scheme/Lisp syntax
 *
 * @return value of the token if type matches, or `NULL` otherwise
 *
 * @see getnc()
 * @see token.h
 *
 * @note this function modifies the static values in of getnc()
 */
string_t read_number(const string_t code) {
    string_t vbuffer = malloc(TOK_SIZE_LIMIT * sizeof(char));
    int i = 0;
    bool period_found = false;
    char c;

    while (!isspace(c = getnc(code)) && c != ')') {
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

/**
 * reads the token value from `code` as symbol
 *
 * @param code a Scheme/Lisp syntax
 *
 * @return value of the token if type matches, or `NULL` otherwise
 *
 * @see getnc()
 * @see token.h
 *
 * @note this function modifies the static values in of getnc()
 */
string_t read_symbol(const string_t code) {
    string_t vbuffer = malloc(TOK_SIZE_LIMIT * sizeof(char));
    string_t not_allowed = "()\'\"\\;";
    char c;
    int i = 0;

    while (!isspace(c = getnc(code)) && c != ')') {
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
