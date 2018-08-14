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

void raise_error(FILE *stream, string_t errmsg);

char stream_char(const string_t str, bool_t isget);
char ungetnc(const string_t str);
char getnc(const string_t str);
string_t reduce_string_size(string_t str);

#endif				/* __SCMIN_MAIN_H */
