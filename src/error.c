/**
 * @file error.c
 *
 * @brief declaration of error handling functionalities
 *
 * err_raise() is called in the different stages of evaluations, indeed
 * while lexing, parsing and evaluating. err_log() would print the
 * occurring errors.
 */

#include "main.h"
#include "vector.h"
#include "chars.h"

/**
 * @brief the error log of the occurred errors
 * @see error.c
 */
vector_t *error_log = NULL;

/**
 * @brief initializing errors and their messages
 * @see main.h
 */
error_t errs[] = {
    {"PARENS ARE NOT BALANCED", NULL, ERR_PRNS_BLNC},
    {"STARTING WITH A CLOSING PAREN", NULL, ERR_PRNS_CLS},

    {"TOKEN IS NOT CORRECT", NULL, ERR_TOK_ERR},
    {"UNEXPECTED EOF IS REACHED", NULL, ERR_EOF_ERR},
    {"SIZE LIMIT IS REACHED", NULL, ERR_SIZE_ERR},
    {"SYMBOL CONTAINS BAD CHARACTERS", NULL, ERR_SYM_ERR},

    {"NUMBER HAS MULTIPLE SIGNS", NULL, ERR_NUM_SIGN},
    {"NUMBER HAS MULTIPLE PERIODS", NULL, ERR_NUM_PRD},
    {"DIVIDING BY ZERO", NULL, ERR_DIVID_ZERO},
    {"NUMBER HAS ALPHA CHARACTERS", NULL, ERR_NUM_DIG},

    {"ARGUMENT TYPE IS NOT CORRECT", NULL, ERR_ARG_TYPE},
    {"ARGUMENT COUNT IS NOT CORRECT", NULL, ERR_ARG_COUNT},
    {"CANNOT BIND LAMBDA ARGS", NULL, ERR_LMBD_ARGS},

    {"FINAL RESULT SHOULD NOT BE NULL", NULL, ERR_RSLT_NULL},
    {"FILE NOT FOUND", NULL, ERR_FILE_ERR},

    {"CANNOT SET SYMBOL", NULL, ERR_CANNOT_SET},
    {"OPERATOR NOT FOUND", NULL, ERR_OP_NOT_FOUND},
    {"CANNOT ALTER RESERVED SYMBOLS OR LITERALS", NULL, ERR_MDFY_RSRVD},

    {NULL, NULL, ERR_NO_ERROR}
};

void err_raisee(serror_t err, bool cond, int line, string_t file, string_t msg) {
    int i;

    /* initializing the error log */
    if (error_log == NULL)
	error_log = vector_new(err_free, err_print, NULL);

    /* if the condition is false then stop here */
    if (!cond)
	return;

    /* find the error and push it to the log */
    for (i = 0; errs[i].errmsg; ++i)
	if (errs[i].err == err) {
	    errs[i].cond = malloc(0x0584);
	    snprintf(errs[i].cond, 0x0584, "(%s) at %s:%d",
		     msg, file, line);
	    reduce_string_size(errs[i].cond);
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
    error_t *err = o;

    if (err)
	free(err->cond);
}

/**
 * @brief outputs an error on the screen
 * @param o the error
 */
void err_print(object_t o) {
    if (o == NULL)
	return;

    error_t *r = o;
    fprintf(stdout, "ERROR%d: %s %s", r->err, r->errmsg, r->cond);
}
