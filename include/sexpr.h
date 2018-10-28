#ifndef _SCMIN_SEXPR_H
#  define _SCMIN_SEXPR_H

/**
 * @file sexpr.h
 *
 * @brief definitions of s-expression and its types, lambda
 *
 * provides functionalities to create and identify s-expression of any type
 *
 * @see types.h
 */

#  include "main.h"
#  include "gc.h"

typedef double number_t;
typedef char *string_t;

/**
 * @brief possible types of a s-expression
 */
typedef enum SYMBOLIC_EXPRESSION_TYPE {
    /**
     * @brief a cons-cell pair; car, cdr
     */
    T_PAIR,

    /**
     * @brief a number 0 -100 0.25
     */
    T_NUMBER,

    /**
     * @brief a "string"
     */
    T_STRING,

    /**
     * @brief a symbol, such as foo or foo-bar
     */
    T_SYMBOL,

    /**
     * @brief (lambda (args) (body))
     */
    T_LAMBDA,

    /**
     * @brief like NULL
     */
    T_NIL,

    /**
     * @brief ERROR flag
     */
    T_ERR
} type_t;

/**
 * @brief the lambda expression is an expression that takes
 * expressions as arguments, i.e. a function
 *
 * @note lambdas are defined as (lambda (args) (body))
 * @note union is used to manage memory efficiently
 */
typedef struct LAMBDA_EXPRESSION {
    /**
     * @brief garbage collector information
     */
    gc_info gci;

    /**
     * @brief the parent scope of the lambda
     * @note this would be used if some symbole was bonded to the
     * `parent` and was used within the lambda
     */
    scope_t *parent;

    /**
     * @brief the lambdas arguments (un-bonded symbols only)
     */
    sexpr_t *args;

    /**
     * @brief true if the lambda is defined nativly in C
     */
    bool isnative;


    /**
     * @brief the lambda could be either native or a body as s-expression
     */
    union {
	/**
	 * @brief native lambda defined in C
	 */
	native_t *native;

	/**
	 * @brief the lambda's body
	 */
	sexpr_t *body;
    };
} lambda_t;

/**
 * @brief a Lisp/Scheme s-expression contain it's type and the correspondant
 * field
 *
 * @note the usage of union is to use memory efficiently
 */
typedef struct SYMBOLIC_EXPRESSION {
    /**
     * @brief garbage collector information
     */
    gc_info gci;

    /**
     * @brief s-expression type
     */
    type_t type;

    /**
     * @brief possible s-expression since each type require a differnt
     * data type to hold data
     */
    union {
	/**
	 * @brief string/atom
	 */
	string_t s;

	/**
	 * @brief number
	 */
	number_t n;

	/**
	 * @berif cons cell pair car/cdr
	 */
	pair_t *c;

	/**
	 * @brief lambda expression
	 */
	lambda_t *l;
    };
} sexpr_t;

bool isnil(sexpr_t * expr);
bool istrue(sexpr_t * expr);
bool isatom(sexpr_t * expr);
bool isnumber(sexpr_t * expr);
bool isstring(sexpr_t * expr);
bool issymbol(sexpr_t * expr);
bool islambda(sexpr_t * expr);
bool ispair(sexpr_t * expr);
bool islist(sexpr_t * expr);
bool isnative(sexpr_t * expr);

sexpr_t *sexpr_new(type_t type);
void sexpr_describe(object_t expr);
void sexpr_print(object_t expr);
int sexpr_length(sexpr_t * expr);

sexpr_t *sexpr_err(void);
sexpr_t *sexpr_nil(void);
sexpr_t *sexpr_true(void);

sexpr_t *lambda_new_native(scope_t * parent, sexpr_t * args,
			   native_t * func);
sexpr_t *lambda_new(scope_t * parent, sexpr_t * args, sexpr_t * body);
void lambda_describe(object_t expr);
void lambda_print(object_t expr);

#endif				/* _SCMIN_SEXPR_H */
