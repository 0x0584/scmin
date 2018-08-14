#include "../include/token.h"

token_t *token_new(token_type type, string_t str, int depth) {
    token_t *token = malloc(sizeof *token);

    token->type = type;
    token->buffer = str;
    token->depth = depth;

    return token;
}

void token_print(object_t t) {
    assert(t);

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
    case TOK_STRING:
	str = "TOK_STRING";
	break;
    case TOK_EOF:
	str = "TOK_EOF";
	break;
    default:
    case TOK_ERR:
	str = "TOK_EOF";
	break;
    }

    puts("depth \t    type \t sexpr");
    printf("%.2d \t %s \t %8s\n", foo->depth, str, foo->buffer);
}

void token_free(object_t o) {
    /* assert(((token_t *) o)->buffer != NULL); */

    if (((token_t *) o)->buffer != NULL)
	free(((token_t *) o)->buffer);
    free(o);
}
