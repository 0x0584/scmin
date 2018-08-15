/**
 * @file characters.c
 *
 * this file contains declaractions of useful character handling functionalities
 * to help simulating string as a stream of characters so that we can keep an
 * eye on the last character we have been on last time.
 */

#include "../include/characters.h"

/**
 * stream_string() holds two static variables, one for the old string and one
 * for the current index. if oldstr == @p str that means that the string is
 * the same as the previouis time and  we return  the current character and
 * then increment index. otherwise, we set the oldstr to str, and we return
 * the first character. if false, we return than this is the old string, we
 * look for the isget. if @p isget was true, * return the cuurent character,
 * otherwise, resent the character to the stream by * decrementing the index
 *
 * @param str a string to keeo track on
 * @param isget take or push back current character
 *
 * @return the desired character
 */
char stream_string(const string_t str, bool_t isget) {
    static int i = 0;
    static string_t oldstr = NULL;

#if defined LEXER_DEBUG
    assert(str != NULL);
#endif

    if (!str || !str[i]) {
	i = 0;
	oldstr = NULL;
	return EOF;
    } else if (oldstr != str) {
	i = 0;
	oldstr = str;
    }

    if (isget) {
	char c;

	if ((c = str[i++])) {
	    return c;
	} else {
	    return EOF;
	}
    } else if (!isget && i > 0) {
	return oldstr ? oldstr[i--] : EOF;
    } else {
	return str[i];
    }
}

string_t stream_as_string(FILE * stream) {
    fputc('\0', stream);
    return "";
}

char ungetnc(void) {
    return stream_string("NIL", false);
}

char getnc(const string_t str) {
    return stream_string(str, true);
}

string_t reduce_string_size(string_t str) {
    return realloc(str, strlen(str) * sizeof(char));
}
