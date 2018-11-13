/**
 * @file error.c
 *
 * @brief declaration of error handling functionalities
 *
 * err_raise() is called in the different stages of evaluations, indeed
 * while lexing, parsing and evaluating. err_log() would print the
 * occurent errors.
 */

#include "main.h"
#include "vector.h"

/**
 * @brief the error log of the occured errors
 * @see err_log()
 */
vector_t *error_log = NULL;

/**
 * @brief initializing errors and their messages
 * @see main.h
 */
error_t errs[] = {
    {"PARENS ARE NOT BALANCED", ERR_PRNS_BLNC},
    {"STARTING WITH A CLOSING PAREN", ERR_PRNS_CLS},

    {"TOKEN IS NOT CORRECT", ERR_TOK_ERR},
    {"UNEXPECTED EOF IS REACHED", ERR_EOF_ERR},
    {"SIZE LIMIT IS REACHED", ERR_SIZE_ERR},
    {"SYMBOL CONTAINS BAD CHARACTERS", ERR_SYM_ERR},

    {"NUMBER HAS MULTIPLE SIGNS", ERR_NUM_SIGN},
    {"NUMBER HAS MULTIPLE PERIODS", ERR_NUM_PRD},
    {"DIVIDING BY ZERO", ERR_DIVID_ZERO},
    /* FIXME: probably parse this as symbol */
    {"NUMBER HAS ALPHA CHARACTERS", ERR_NUM_DIG},

    {"ARGUMENT TYPE IS NOT CORRECT", ERR_ARG_TYPE},
    {"ARGUMENT COUNT IS NOT CORRECT", ERR_ARG_COUNT},
    {"CANNOT BIND LAMBDA ARGS", ERR_LMBD_ARGS},

    {"FINAL RESULT SHOULD NOT BE NULL", ERR_RSLT_NULL},
    {"FILE NOT FOUND", ERR_FILE_ERR},

    {"CANNOT SET SYMBOL", ERR_CANNOT_SET},
    {"OPERATOR NOT FOUND", ERR_OP_NOT_FOUND},

    {NULL, ERR_NO_ERROR}
};

/**
 * @brief if `cond` is true, then raise `err`
 *
 * `err` should be one of the predefined errors
 *
 * @param err the error to raise
 * @param cond true or false
 *
 * @see #error_log
 */
void err_raise(serror_t err, bool cond) {
    int i;

    /* initializing teh error log */
    if (error_log == NULL)
	error_log = vector_new(err_free, err_print, NULL);

    /* if the condition is false then stop here */
    if (!cond)
	return;

    /* find the error and push it to the log */
    for (i = 0; errs[i].errmsg; ++i)
	if (errs[i].err == err) {
	    vector_push(error_log, &errs[i]);
	    break;
	}
}

/**
 * @brief prints the list of raised errors
 *
 * @return the number of errors
 * @note #error_log is free'd after calling this function
 */
int err_log(void) {
    if (!error_log)
	return 0;

    int size = error_log->size;

    vector_print(error_log);
    err_clean();

    return size;
}

/**
 * @brief frees the #error_log and set it to `NULL`
 */
void err_clean(void) {
    vector_free(error_log);
    error_log = NULL;
}

/**
 * @brief does nothing
 * @param o the error
 *
 * @note just to create a vector properly
 * @see vector.c
 */
void err_free(object_t o) {
    if (o)
	return;
}

/**
 * @brief outputs an error on the screen
 * @param o the error
 */
void err_print(object_t o) {
    if (o == NULL)
	return;

    error_t *r = o;
    fprintf(stdout, "ERROR%d: %s", r->err, r->errmsg);
}
