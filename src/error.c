/*
 * this would have raise_error() to handle error and a set of
 * predefined error numbers that would be classifier according
 * to phases
 *
 *   + lexing
 *   + parsing
 *   + evaluation
 */

#include "../include/main.h"
#include "../include/vector.h"

static vector_t *errlog = NULL;
static error_t errs[] = {
    {"PARENS ARE NOT BALANCED", ERR_PRNS_BLNC},
    {"STARTING WITH A CLOSING PAREN", ERR_PRNS_CLS},

    {"TOKEN IS NOT CORRECT", ERR_TOK_ERR},
    {"UNEXPECTED EOF IS REACHED", ERR_EOF_ERR},
    {"SIZE LIMIT IS REACHED", ERR_SIZE_ERR},
    {"SYMBOL CONTAINS BAD CHARACTERS", ERR_SYM_ERR},

    {"NUMBER HAS MULTIPLE SIGNS", ERR_NUM_SIGN},
    {"NUMBER HAS MULTIPLE PERIODS", ERR_NUM_PRD},
    /* TODO: parse this as symbol */
    {"NUMBER HAS ALPHA CHARACTERS", ERR_NUM_DIG},

    {"ARGUMENT TYPE IS NOT CORRECT", ERR_ARG_TYPE},
    {"ARGUMENT COUNT IS NOT CORRECT", ERR_ARG_COUNT},
    {"CANNOT BIND LAMBDA ARGS", ERR_LMBD_ARGS},
    {"FINAL RESULT SHOULD NOT BE NULL", ERR_RSLT_NULL},

    {NULL, ERR_NO_ERROR}
};

void err_raise(serror_t err, bool cond) {
    int i;

    if (errlog == NULL)
	errlog = vector_new(err_free, err_print, NULL);

    if (!cond)
	return;

    for (i = 0; errs[i].errmsg; ++i)
	if (errs[i].err == err) {
	    vector_push(errlog, &errs[i]);
	    break;
	}
}

int err_log(void) {
    if(!errlog || errlog->size == 0)
	return 0;
    vector_debug(stdout, errlog);
    err_clean();
    return errlog->size;
}

void err_clean(void) {
    vector_free(errlog);
}


void err_free(object_t o) {
    if (o)
	return;
}

void err_print(object_t o) {
    error_t *r = o;
    fprintf(stdout, "ERROR%d: %s\n", r->err, r->errmsg);
}
