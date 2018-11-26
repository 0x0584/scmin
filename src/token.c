/**
 * @file token.c
 *
 * @brief token functionalities needed by the lexing functions
 *
 * @see lexer.h
 * @see token.h
 * @see chars.h
 */

#include "token.h"
#include "chars.h"

/**
 * @brief predicts the type of a token
 *
 * @param code to be passed to getnc()
 *
 * @return a token type
 *
 * @see chars.h
 */
token_type predict_token_type(string_t code) {
    token_type type = TOK_ERR;
    char c;

    /* handling lists and literal strings */
    switch (c = getnc(code)) {
    case '(':			/* beginning of a list */
	type = TOK_L_PAREN;
	goto RET;
    case ')':			/* end of a list */
	type = TOK_R_PAREN;
	goto RET;
    case '\'':			/* quoted list */
	type = TOK_QUOTE;
	goto RET;
    case '\"':			/* literal string */
	type = TOK_STRING;
	goto RET;

    default:
	break;
    };

    /* if we made it up to here, token must be either a number
     * or a symbol. at first, we suppose it's a number unless
     * we found out the opposite */

    int count = 1;		/* counting how many getnc() */

    type = TOK_NUMBER;

    if (isdigit(c) || strchr(".-+", c)) {
	bool period = false, sign = false;
	char cc = 0x00;

	while (true) {
	    if (c == '+' || c == '-') {
		/* signs must appear only once at the beginning and
		 * not followed with either a space of left parenthesis,
		 * otherwise this is a symbol */

		if (count == 1)
		    sign = true;
		else if (count != 1 || sign) {
		    type = TOK_SYMBOL;
		    break;
		}

		cc = getnc(code), ungetnc();

		if (isspace(cc) || cc == ')') {
		    type = TOK_SYMBOL;
		    break;
		}
	    } else if (c == '.') {
		/* periods only appear once
		 * otherwise this is a symbol */
		if (!period)
		    period = true;
		else {
		    type = TOK_SYMBOL;
		    break;
		}
	    } else if (isspace(c) || c == ')') {
		break;
	    } else if (!isdigit(c)) {
		type = TOK_SYMBOL;
		break;
	    }

	    c = getnc(code), count++;
	}
    } else {
	type = TOK_SYMBOL;
    }

    /* this behavior is a side effect of reading so many characters,
     * so calling ungentc() to return them back */
    while (count--)
	ungetnc();

  RET:
    return type;
}

/**
 * @brief allocate and initialize a new token
 *
 * @param type one of the types in #TOKEN_TYPE
 * @param vbuffer token as text
 * @param depth how many parenthesis are there
 *
 * @return the initialized token
 */
token_t *token_new(token_type type, string_t vbuffer, int depth) {
    token_t *token = (token_t *) gc_malloc(sizeof(token_t));
    memset(token, 0, sizeof(token_t));

    token->type = type;
    token->vbuffer = vbuffer;
    token->depth = depth;

    return token;
}

/**
 * @brief display the token to the screen
 *
 * display the token as DEPTH - TYPE - BUFFER to the stdout using
 * stdio printf
 *
 * @param o the token to print
 *
 * @see vector.h
 * @note the reason why the token's type here is object_t instead of
 * token is because this is used as printing function by vector_print()
 */
void token_print(object_t o) {
    if (o == NULL)
	return;

    char *type_str = NULL;
    token_t *token = (token_t *) o;

    switch (token->type) {
    case TOK_L_PAREN:
	type_str = "TOK_L_PAREN";
	break;
    case TOK_R_PAREN:
	type_str = "TOK_R_PAREN";
	break;
    case TOK_QUOTE:
	type_str = "TOK_QUOTE";
	break;
    case TOK_STRING:
	type_str = "TOK_STRING";
	break;
    case TOK_SYMBOL:
	type_str = "TOK_SYMBOL";
	break;
    case TOK_NUMBER:
	type_str = "TOK_NUMBER";
	break;
    case TOK_EOL:
	type_str = "TOK_EOL";
	break;
    default:
    case TOK_ERR:
	type_str = "TOK_ERR";
	break;
    }

    printf(" (depth:%.2d) - (type:%11s) - (value:%s)\n",
	   token->depth, type_str, token->vbuffer);
}

/**
 * @brief free the memory occupied by token
 *
 * it frees also the token's buffer if it was not @p NULL
 *
 * @param o the token to print
 *
 * @see vector.h
 * @note the reason why the token's type here is object_t instead of
 * token is because this is used as printing function by vector_print()
 */
void token_free(object_t o) {
    if (o == NULL)
	return;

    token_t *token = o;

    if (token->vbuffer != NULL)
	free(token->vbuffer);
    free(token);
}
