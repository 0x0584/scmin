/**
 * @file characters.c
 *
 * this file contains declarations of useful character handling functionalities
 * to help simulating string as a stream of characters so that we can keep an
 * eye on the last character we have been on last time.
 */

#include "../include/characters.h"

/**
 * stream_string() holds two static variables, one for the old string and one
 * for the current index. if oldstr == @p str that means that the string is
 * the same as the previous time and  we return	 the current character and
 * then increment index. otherwise, we set the oldstr to str, and we return
 * the first character. if false, we return than this is the old string, we
 * look for the isget. if @p isget was true, * return the current character,
 * otherwise, resent the character to the stream by --index
 *
 * @param str a string to keep track on
 * @param isget take or push back current character
 *
 * @return the desired character
 * @note string must not be NULL
 */
char stream_string(const string_t str, bool isget) {
    static int index = 0;
    static char *oldstr = NULL;

    /* string must not be NULL */
    if (str == NULL)
	return EOF;

    /* if this is a different string, use the new string */
    if (str != oldstr && isget) {
	index = 0;
	oldstr = str;
    }

    if (isget) {		/* getnc() stream forward */
	if (!oldstr[index]) {	/* set everything back again after '\0' */
	    index = 0;
	    oldstr = NULL;
	    return EOF;
	}

	return oldstr[index++];	/* move the index to the next character */
    } else {			/* ungetnc() stream backward */
	if (index > 0)
	    return oldstr[index--];
	else if (index == 0)
	    return oldstr[index];
	else
	    return EOF;		/* not reachable */
    }
}

/* todo: write this to read the files */
string_t stream_as_string(const char *filename) {
    static char *buffer = NULL;
    FILE *handler = NULL;

    err_raise(ERR_FILE_ERR, !(handler = fopen(filename, "r")));

    if (err_log())
	return NULL;

    int string_size, read_size;

    fseek(handler, 0, SEEK_END); /* Beginning of the stream */
    string_size = ftell(handler); /* Size of the stream */
    rewind(handler);

    buffer = (char*) malloc( sizeof(char) * (string_size + 1) );

    /* Binary size of the stream */
    read_size = fread(buffer, sizeof(char), string_size, handler);
    buffer[string_size] = '\0';

    /* The readied size is != to stream size */
    if (string_size != read_size) {
	free(buffer);
	buffer = NULL;
    }

    fclose(handler);

    return buffer;
}

/**
 * basically calling stream_string() with @p str and pass isget as true
 *
 * @see stream_string()
 *
 * @return the next character in the stream
 */
char getnc(const string_t str) {
    return stream_string(str, true);
}

/**
 * basically calling stream_string() with @p str and pass isget as false
 *
 * @see stream_string()
 *
 * @return the previously streamed character in the stream
 */
char ungetnc(void) {
    return stream_string("0x0584", false);
}

/**
 * takes a dynamically allocated string, and reduce it's size to fit
 * the optimal size, i.e. its length plus the null character '\0'
 *
 * @param str a dynamically allocated string
 *
 * @return optimal-size string
 */
string_t reduce_string_size(string_t str) {
    return realloc(str, (1 + strlen(str)) * sizeof(char));
}
