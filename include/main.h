#ifndef __SCMIN_MAIN_H
#  define __SCMIN_MAIN_H

#  include <stdio.h>
#  include <stdlib.h>
#  include <string.h>
#  include <ctype.h>
#  include <math.h>
#  include <assert.h>
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
#  define GC_DEBUG	DBG_OFF

#  define VECTOR_DEBUG	DBG_OFF
#  define LEXER_DEBUG	DBG_OFF
#  define PARSER_DEBUG	DBG_ON

/**
 * the boolean type
 */
enum BOOLEAN {
    false = (1 == 0),
    true = !false
};

/**
 * for now this just print the err_msg
 *
 * FIXME: detect the error_level and then decide whether to quit or not
 */
void raise_error(FILE * stream, string_t errmsg);

#endif				/* __SCMIN_MAIN_H */
