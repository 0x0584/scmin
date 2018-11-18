/**
 * @file chars.c
 *
 * @brief contains declarations of useful character handling functionalities.
 *
 * the essence of those functions is to help simulating string as a stream of
 * characters so that we can keep an eye on the last character we have
 * been on last time.
 */

#include "chars.h"

/**
 * @brief when it's called, if the same parameter `str` was passed, it
 * would return the characters of `str` successively each time.
 *
 * holds two static variables, one for the old string and one for the
 * current index. if `oldstr == str` that means that the string is the
 * same as the previous time and  we return the current character and
 * then increment index. otherwise, we set the `oldstr` to `str`, and
 * we return the first character. if `false`, we return than this is the
 * old string, we look for the `isget`. if `isget` was `true`, return
 * the current character, otherwise, decrement the index
 *
 * @param str a string to keep track on
 * @param isget take or push back current character
 *
 * @return the desired character
 *
 * @note string must not be `NULL`
 */
char stream_string(const string_t str, bool isget) {
    static int index = 0;
    static char *oldstr = NULL;

    /* string must not be NULL */
    if (str == NULL)
	return EOF;

    /* if this is a different string, use the new string */
    if (str != oldstr && isget) {
	index = 0, oldstr = str;
    }

    if (isget) {		/* getnc() stream forward */
	if (!oldstr[index]) {	/* set everything back again after '\0' */
	    index = 0, oldstr = NULL;
	    return EOF;
	}

	return oldstr[index++];	/* move the index to the next character */
    } else {			/* ungetnc() stream backward */
	if (oldstr == NULL)
	    return EOF;		/* not reachable */
	else if (index > 0)
	    return oldstr[index--];
	else
	    return oldstr[index];
    }
}

/**
 * @brief takes input from the stream and format it as a string
 *
 * @param filename to get characters from
 *
 * @return the content of what was typed
 * @note it stops after hitting `(\r || \n)`
 */
string_t file_as_string(const char *filename) {
    static char *buffer = NULL;
    FILE *handler = NULL;

    err_raise(ERR_FILE_ERR, !(handler = fopen(filename, "r")));

    if (err_log())
	return NULL;

    int string_size, read_size;

    fseek(handler, 0, SEEK_END);	/* Beginning of the stream */
    string_size = ftell(handler);	/* Size of the stream */
    rewind(handler);

    buffer = (char *) malloc(sizeof(char) * (string_size + 1));

    /* binary size of the stream */
    read_size = fread(buffer, sizeof(char), string_size, handler);
    buffer[string_size] = '\0';

    /* the readied size is != to stream size */
    if (string_size != read_size) {
	free(buffer);
	buffer = NULL;
    }

    fclose(handler);

    return buffer;
}

string_t stdin_as_string(void) {
    const size_t INPUT_SIZE_LIMIT = (2 << 10);
    string_t buffer = malloc(INPUT_SIZE_LIMIT * sizeof(char));
    short index = 0, c;

    while ((c = getchar())) {
	if (c == EOF && index == 0) {
	    free(buffer);
	    return NULL;
	} else if (c == EOF && index != 0) {
	    free(buffer);
	    buffer = malloc(sizeof(char));
	    *buffer = '\0';
	    return buffer;
	} else if (c == '\n' || c == '\r') {
	    buffer[index] = '\0';
	    break;
	} else {
	    buffer[index++] = c;
	}
    }

    return reduce_string_size(buffer);
}

/**
 * @brief basically calling stream_string() with `str` and
 * pass `isget` as `true`
 *
 * @return the next character in the stream
 *
 * @see stream_string()
 */
char getnc(const string_t str) {
    return stream_string(str, true);
}

/**
 * @brief basically calling stream_string() with `str` and
 * pass `isget` as `false`
 *
 * @return the previously streamed character in the stream
 *
 * @see stream_string()
 */
char ungetnc(void) {
    return stream_string("0x0584", false);
}

/**
 * @brief reallocate the memory so that the size of `str` matches strlen()
 * takes a dynamically allocated string, and reduce it's size to fit
 * the optimal size, i.e. its length plus the null character `\0`
 *
 * @param str a dynamically allocated string
 *
 * @return optimal-size string
 */
string_t reduce_string_size(string_t str) {
    return realloc(str, (1 + strlen(str)) * sizeof(char));
}

/**
 * @brief takes any possible white-spaces from the `code` string
 *
 * @details this is really setting the index in getnc() to the first
 * non-white-spaced character
 *
 * @param code a string containing Scheme/Lisp syntax
 *
 * @return `false` if we reach the `EOF`
 *
 * @see gentc()
 */
bool clean_whitespaces(string_t code) {
    char c;

    while ((c = getnc(code)) != EOF && isspace(c));

    if (c != EOF)
	ungetnc();

    return c != EOF;
}

/**
 * @brief takes any possible comments from the `code` string
 *
 * @details this is really setting the index in getnc() to the line
 * not starting with `;`
 *
 * @param code a string containing Scheme-like syntax
 *
 * @return `false` if we reach the `EOF`
 *
 * @see gentc()
 */
bool clean_comments(string_t code) {
    char c;

    while ((c = getnc(code)) == ';')
	do
	    c = getnc(code);
	while (c != '\n' && c != '\r' && c != EOF);

    if (c != EOF)
	ungetnc();

    return c != EOF;
}


bool clean_source_code(string_t code) {
    char c = 0x00;

  CLEAN: /* this is a weird way to write some code but labels are
	  * great to create loops, right? */
    if (!clean_whitespaces(code) || !clean_comments(code))
	return false;

    c = getnc(code), ungetnc();

    /* it's possible to have spaces and comments after each other
     * thus we have to test again */
    if (isspace(c) || c == ';')
	goto CLEAN;

    return true;
}
