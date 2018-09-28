/**
 * @file token.c
 *
 * the main function here is predict_token_type() to guess the type
 * of the token
 */

#include "../include/token.h"
#include "../include/characters.h"

token_type predict_token_type(string_t code) {
    token_type type = TOK_ERR;
    char c;
    /* printf("\n>>> %c\n", c); */

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

    if (isdigit(c) || strchr(".-+", c)) {
	char cc = getnc(code);

	/* next character must be a space/digit
	 * otherwise it's a symbol */
	if ((isdigit(cc) || isspace(cc) || cc == ')') && !strchr(".-+", c)) {
	    type = TOK_NUMBER;
	} else {
	    type = TOK_SYMBOL;
	}

	ungetnc();
    } else {
	type = TOK_SYMBOL;	/* symbol */
    }

    ungetnc();

  RET:
    return type;
}

token_t *token_new(token_type type, string_t vbuffer, int depth) {
    token_t *token = malloc(sizeof *token);

    token->type = type;
    token->vbuffer = vbuffer;
    token->depth = depth;

    return token;
}

void token_print(object_t t) {
#if defined LEXER_DEBUG
    assert(t != NULL);
#endif

    char *type_str = NULL;
    token_t *token = (token_t *) t;

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
    case EOL:
	type_str = "EOL";
	break;
    default:
    case TOK_ERR:
	type_str = "TOK_EOF";
	break;
    }

    printf(" (depth:%.2d) - (type:%11s) - (value:%s)\n",
	   token->depth, type_str, token->vbuffer);
}

void token_free(object_t o) {
#if LEXER_DEBUG
    token_t *t = o;
    /* static int i = 0; */

    if (t->type != TOK_L_PAREN && t->type != TOK_R_PAREN
	&& t->type != TOK_QUOTE && t->type != EOL) {
	assert(t->vbuffer != NULL);
    }
#endif

    if (((token_t *) o)->vbuffer != NULL)
	free(((token_t *) o)->vbuffer);
    free(o);
}
