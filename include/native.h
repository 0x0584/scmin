#ifndef _SCHMIN_NATIVE
#  define _SCHMIN_NATIVE

#  include "types.h"
#  include "main.h"

/**
 * @brief a native lambda is a symbol and native C function
 *
 * @details native lambdas ad essentials to add basic features
 * to the interpreter like `+` or `and`
 */
typedef struct LAMBDA_NATIVE {
    /**
     * @brief a symbol to be bounded with
     */
    string_t symbol;

    /**
     * @brief a C function that plays the role of a lambda
     *
     * @param args a list or arguments
     * @return evaluated expression
     *
     * @see native.c
     */
    sexpr_t *(*func) (sexpr_t *args);
} nlambda_t;

sexpr_t *native_add(sexpr_t * expr);
sexpr_t *native_minus(sexpr_t * expr);
sexpr_t *native_times(sexpr_t * expr);
sexpr_t *native_divid(sexpr_t * expr);
sexpr_t *native_eq(sexpr_t * expr);
sexpr_t *native_less(sexpr_t * expr);
sexpr_t *native_greater(sexpr_t * expr);
sexpr_t *native_less_eq(sexpr_t * expr);
sexpr_t *native_greater_eq(sexpr_t * expr);
sexpr_t *native_sqrt(sexpr_t * expr);
sexpr_t *native_square(sexpr_t * expr);

sexpr_t *native_or(sexpr_t * expr);
sexpr_t *native_and(sexpr_t * expr);
sexpr_t *native_iseq(sexpr_t * expr);
sexpr_t *native_isatom(sexpr_t * expr);
sexpr_t *native_list(sexpr_t * expr);
sexpr_t *native_length(sexpr_t * expr);
sexpr_t *native_cons(sexpr_t * expr);
sexpr_t *native_car(sexpr_t * expr);
sexpr_t *native_cdr(sexpr_t * expr);
sexpr_t *native_set_car(sexpr_t * expr);
sexpr_t *native_set_cdr(sexpr_t * expr);
sexpr_t *native_print(sexpr_t * expr);
sexpr_t *native_isnil(sexpr_t * expr);
sexpr_t *native_istrue(sexpr_t * expr);
sexpr_t *native_isstring(sexpr_t * expr);
sexpr_t *native_isnumber(sexpr_t * expr);
sexpr_t *native_issymbol(sexpr_t * expr);
sexpr_t *native_islambda(sexpr_t * expr);
sexpr_t *native_islist(sexpr_t * expr);

#endif				/* _SCHMIN_NATIVE */
