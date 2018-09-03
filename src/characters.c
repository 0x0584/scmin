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
 *
 * FIXME: try to keep track of multiple string at once
 */
char stream_string(const string_t str, bool isget) {
    static int index = 0;
    static char *oldstr = NULL;

    /* string must not be NULL */
    if (!str) {
	return EOF;
    }

    /* if this is a different string, use the new string */
    if (str != oldstr && isget) {
	oldstr = str;
	index = 0;
    }

    if (isget) {		/* getnc() stream forward */
	if (!oldstr[index]) {	/* we have reached the end of the string
				 * so set everything back again */
	    index = 0;
	    oldstr = NULL;
	    return EOF;
	}

	return oldstr[index++];	/* move the index to the next character */
    } else {			/* ungetnc() stream backward */
	if (index > 0) {
	    return oldstr[index--];
	} else if (index == 0) {
	    return oldstr[index];
	} else {
	    return EOF;		/* not reachable */
	}
    }
}

/* todo: write this to read the files */
string_t stream_as_string(FILE * stream) {
    fputc('\0', stream);

    return "";
}

char ungetnc(void) {
    return stream_string("0x0584", false);
}

char getnc(const string_t str) {
    return stream_string(str, true);
}

string_t reduce_string_size(string_t str) {
    return realloc(str, 1 + strlen(str) * sizeof(char));
}
