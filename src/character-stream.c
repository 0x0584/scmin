#include "../include/main.h"

char stream_char(const string_t str, bool_t isget) {
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

//    printf("[%d] %c%c%c\n", i, str[i - 1], str[i], str[i + 1]);

    if (isget) {
	char c;

	if ((c = str[i++])) {
	    return c;
	} else {
	    return EOF;
	}
    } else if (!isget && i > 0) {
	return str[i--];
    } else {
	return str[i];
    }
}

char ungetnc(const string_t str) {
    return stream_char(str, false);
}

char getnc(const string_t str) {
    return stream_char(str, true);
}

string_t reduce_string_size(string_t str) {
    return realloc(str, strlen(str) * sizeof(char));
}
