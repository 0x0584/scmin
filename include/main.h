#ifndef __SCMIN_MAIN_H
#  define __SCMIN_MAIN_H

/**
 * @file main.h
 *
 * @brief definition of debugging symbols and error types
 */

#  include <assert.h>

#  include <stdio.h>
#  include <stdlib.h>
#  include <stdbool.h>

#  include <math.h>

#  include <string.h>
#  include <ctype.h>

#  include <time.h>

#  include "types.h"

/**
 * @brief debugging is ON
 */
#  define DBG_ON	(0)

/**
 * @brief debugging is OFF
 */
#  define DBG_OFF	(1)

#  define DEBUG_FULL		DBG_ON

/**
 * @brief garbage collector debugging information
 * @see gc.c
 */
#  define DEBUG_GC		DBG_ON

/**
 * @brief vector debugging information
 * @see vector.c
 */
#  define DEBUG_VECTOR		DBG_OFF

/**
 * @brief lexer debugging information
 * @see lexer.c
 */
#  define DEBUG_LEXER		DBG_ON

/**
 * @brief parser debugging information
 * @see parser.c
 */
#  define DEBUG_PARSER		DBG_OFF

/**
 * @brief eval debugging information
 * @see eval.c
 */
#  define DEBUG_EVALUATOR	DBG_ON

/**
 * @brief repl debugging information
 * @see repl.c
 */
#  define DEBUG_REPL		DBG_OFF

/**
 * @brief the interpreter's standard Scheme/Lisp library
 *
 * this file contains many essential functions written in Scheme/Lisp
 */
#  define STD_SCHEME_LIB	"stdlib.scm"

/**
 * @brief possible errors to catch by the interpreter
 *
 * different errors that would occur during the process of evaluating
 * a s-expression
 */
typedef enum SCHEME_ERROR {
    /**
     * @brief this indicates that there is no error;
     * everything is fine
     */
    ERR_NO_ERROR = -1,

    /**
     * @brief no closing parenthesis is found
     * @see lexer.c
     */
    ERR_PRNS_CLS,

    /**
     * @brief parenthesis are not balanced
     * @see lexer.c
     */
    ERR_PRNS_BLNC,

    /**
     * @brief token error; general error
     * @see lexer.c
     * @see token.c
     */
    ERR_TOK_ERR,

    /**
     * @brief unexpected END OF FILE occurred
     * @see lexer.c
     * @see characters.c
     */
    ERR_EOF_ERR,

    /**
     * @brief token passed the size limit #TOK_SIZE_LIMIT
     * @see lexer.c
     */
    ERR_SIZE_ERR,

    /**
     * @brief symbol contains some unexpected characters
     * @see lexer.c
     */
    ERR_SYM_ERR,

    /**
     * @brief number contains multiple signs
     * @see lexer.c
     */
    ERR_NUM_SIGN,

    /**
     * @brief number contains multiple periods
     * @see lexer.c
     */
    ERR_NUM_PRD,

    /**
     * @brief number contains illegal characters
     * @see lexer.c
     */
    ERR_NUM_DIG,

    /**
     * @brief dividing by zero
     * @see lexer.c
     */
    ERR_DIVID_ZERO,

    /**
     * @brief the argument type is not correct
     * @see native.c
     */
    ERR_ARG_TYPE,

    /**
     * @brief the arguments count is not correct
     * @see native.c
     */
    ERR_ARG_COUNT,

    /**
     * @brief cannot bind lambda arguments
     * @see eval.c
     */
    ERR_LMBD_ARGS,

    /**
     * @brief result should not be `NULL`
     * @see eval.c
     * @note not sure if this would ever be raised but, better
     * have it than not
     */
    ERR_RSLT_NULL,

    /**
     * @brief could not open the file stream
     * @see characters.c
     */
    ERR_FILE_ERR,

    /**
     * @brief using set on an unbounded symbol
     * @see eval_set()
     */
    ERR_CANNOT_SET,

    /**
     * @brief operator not found
     * @see eval_sexpr()
     */
    ERR_OP_NOT_FOUND,

    /**
     * @brief when modifying reserved words such as numbers ans string literals
     * @todo include constants too (after finding the correct syntax)
     */
    ERR_MDFY_RSRVD
} serror_t;

/**
 * @brief binding an error with the correspondent message
 * @see error.c
 */
typedef struct ERROR {
    /**
     * @brief error message
     */
    char *errmsg;

    /**
     * @brief a defined interpreter errors error
     */
    serror_t err;
} error_t;

void err_raise(serror_t err, bool cond);
void err_free(object_t o);
void err_print(object_t o);
int err_log(void);
void err_clean(void);

#endif				/* __SCMIN_MAIN_H */
