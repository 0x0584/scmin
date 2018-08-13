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

enum BOOLEAN {
    false = (1 == 0),
    true = !false
};

enum ERROR_TYPE {
    LEXER_ERROR,
    PARSER_ERROR,
    EVAL_ERROR
};

void raise_error(FILE *stream, string_t errmsg);

#endif				/* __SCMIN_MAIN_H */
