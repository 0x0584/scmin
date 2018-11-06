#ifndef _SCMIN_REPL_H
#  define _SCMIN_REPL_H

#  include "main.h"
#  include "gc.h"

#  define REPL_PROMPT	">"

void repl(scope_t * scope);
void repl_testing(void);

#endif				/* _SCMIN_REPL_H */
