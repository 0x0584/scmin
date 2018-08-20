#ifndef _SCMIN_PARSER_H
#  define _SCMIN_PARSER_H
#  include "main.h"
#  include "gc.h"

sexpr_t *parse_expression(vector_t * tokens);
sexpr_t *parse_as_list(vector_t * v, int *start);
sexpr_t *parse_as_number(string_t value);
sexpr_t *parse_as_string(string_t value);
sexpr_t *parse_as_atom(string_t value);
sexpr_t *parse_as_boolean(string_t value);

#endif				/* _SCMIN_PARSER_H */
