#ifndef _SCMIN_PARSER_H
#  define _SCMIN_PARSER_H
#  include "main.h"
#  include "gc.h"

sexpr_t *parse_sexpr(vector_t * tokens);
vector_t *parse_sexprs(vector_t * vtokens);
sexpr_t *parse_as_list(vector_t * v);
sexpr_t *parse_as_quote(vector_t * v);
sexpr_t *parse_as_number(string_t value);
sexpr_t *parse_as_string(string_t value);
sexpr_t *parse_as_symbol(string_t value);
sexpr_t *parse_as_boolean(string_t value);

void parser_testing(void);

#endif				/* _SCMIN_PARSER_H */
