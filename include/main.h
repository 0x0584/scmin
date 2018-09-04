#ifndef __SCMIN_MAIN_H
#  define __SCMIN_MAIN_H

#  include <assert.h>

#  include <stdio.h>
#  include <stdlib.h>
#  include <stdbool.h>
#  include <stdarg.h>

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
#  define EVALUATOR_DEBUG	DBG_ON

/**
 * for now this just print the err_msg
 *
 * FIXME: detect the error_level and then decide whether to quit or not
 */
void raise_error(FILE * stream, string_t errmsg);

#endif				/* __SCMIN_MAIN_H */
