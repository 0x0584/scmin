#ifndef __SCMIN_MAIN_H
#  define __SCMIN_MAIN_H

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
 * turn the debugging flag
 */
#  define DBG_ON	(0)
#  define DBG_OFF	(1)

/**
 * debug information
 */
#  define GC_DEBUG		DBG_OFF
#  define VECTOR_DEBUG		DBG_OFF
#  define LEXER_DEBUG		DBG_OFF
#  define PARSER_DEBUG		DBG_OFF
#  define EVALUATOR_DEBUG	DBG_OFF

struct ERROR {
    char *errmsg;

    enum SERROR {
	ERR_PRNS_CLS = 0,
	ERR_PRNS_BLNC,

	ERR_TOK_ERR,
	ERR_EOF_ERR,
	ERR_SIZE_ERR,
	ERR_SYM_ERR,

	ERR_NUM_SIGN,
	ERR_NUM_PRD,
	ERR_NUM_DIG,

	ERR_ARG_TYPE,
	ERR_ARG_COUNT,
	ERR_LMBD_ARGS,
	ERR_RSLT_NULL,

	ERR_NO_ERROR = -1
    } err;
};

void err_raise(serror_t err, bool cond);
void err_free(object_t o);
void err_print(object_t o);
int err_log(void);
void err_clean(void);

#endif				/* __SCMIN_MAIN_H */
