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

/**
 * @brief garbag collector debugging information
 * @see gc.c
 */
#  define GC_DEBUG		DBG_OFF

/**
 * @brief vector debugging information
 * @see vector.c
 */
#  define VECTOR_DEBUG		DBG_OFF

/**
 * @brief lexer debugging information
 * @see lexer.c
 */
#  define LEXER_DEBUG		DBG_OFF

/**
 * @brief parser debugging information
 * @see parser.c
 */
#  define PARSER_DEBUG		DBG_OFF

/**
 * @brief eval debugging information
 * @see eval.c
 */
#  define EVALUATOR_DEBUG	DBG_OFF

/**
 * @brief repl debugging information
 * @see repl.c
 */
#  define REPL_DEBUG		DBG_OFF

/**
 * @brief different errors that would occur during te process of evaluating
 * a s-expression
 */
enum SCHEME_ERROR {
    /**
     * @brief no colosing parenthesis is found
     * @see lexer.c
     */
    ERR_PRNS_CLS = 0,

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
     * @brief unexpected END OF FILE occured
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
     * @brief using set on an unbonded symbol
     */
    ERR_CANNOT_SET,

    ERR_OP_NOT_FOUND,

    /**
     * @brief this indicates that there is no error; everything is fine
     */
    ERR_NO_ERROR = -1
};

/**
 * @brief binding an error with the correspondent message
 * @see error.c
 */
struct ERROR {
    /**
     * @brief error message
     */
    char *errmsg;

    /**
     * @brief a defined Scheme error
     */
    serror_t err;
};

void err_raise(serror_t err, bool cond);
void err_free(object_t o);
void err_print(object_t o);
int err_log(void);
void err_clean(void);

#endif				/* __SCMIN_MAIN_H */
