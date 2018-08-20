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
#  define LEXER_DEBUG DBG_OFF
#  define VECTOR_DEBUG DBG_OFF
#  define GC_DEBUG DGB_ON

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
 * TODO:
 * detect the error_level decide whether you want
 * to quit or just stop here of something like this
 */
void raise_error(FILE *stream, string_t errmsg);

#endif				/* __SCMIN_MAIN_H */
