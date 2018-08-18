/**
 * @file token.c
 *
 * the main function here is predict_token_type() to guess the type
 * of the token
 */

#include "../include/token.h"
#include "../include/characters.h"

token_type predict_token_type(char c) {
    token_type type;

    printf("\n>>> %c\n", c);

    /* handling lists and literal strings */
    switch (c) {
    case '(':			/* beginning of a list */
	type = TOK_L_PAREN;
	puts("done L PAREN");
	goto RET;
    case ')':			/* end of a list */
	type = TOK_R_PAREN;
	puts("done R PAREN");
	goto RET;
    case '\'':			/* quoted list */
	type = TOK_S_QUOTE;
	puts("done S QUOTE");
	goto RET;
    case '\"':			/* literal string */
	type = TOK_D_QUOTE;
	puts("done D QUOTE");
	goto RET;
    default:
	break;
    };

    if (isdigit(c) || strchr(".-+", c)) {	/* number */
	type = TOK_NUMBER;
	ungetnc();
    } else {
	type = TOK_ATOM;	/* atom */
	ungetnc();
    }

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

    char *str = NULL;
    token_t *foo = (token_t *) t;

    switch (foo->type) {
    case TOK_L_PAREN:
	str = "TOK_L_PAREN";
	break;
    case TOK_R_PAREN:
	str = "TOK_R_PAREN";
	break;
    case TOK_S_QUOTE:
	str = "TOK_S_QUOTE";
	break;
    case TOK_D_QUOTE:
	str = "TOK_D_QUOTE";
	break;
    case TOK_LAMBDA:
	str = "TOK_LAMBDA";
	break;
    case TOK_ATOM:
	str = "TOK_ATOM";
	break;
    case TOK_NUMBER:
	str = "TOK_NUMBER";
	break;
    case TOK_EOL:
	str = "TOK_EOL";
	break;
    default:
    case TOK_ERR:
	str = "TOK_EOF";
	break;
    }

    puts("\ndepth \t	type \t	sexpr");
    printf("%.2d \t %s \t %8s\n", foo->depth, str, foo->vbuffer);
}

void token_free(object_t o) {
#if LEXER_DEBUG
    assert(((token_t *) o)->vbuffer != NULL);
#endif

    if (((token_t *) o)->vbuffer != NULL)
	free(((token_t *) o)->vbuffer);
    free(o);
}
