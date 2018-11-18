#ifndef _SCMIN_CHARACTERS_H
#  define _SCMIN_CHARACTERS_H

/**
 * @file chars.h
 *
 * @brief definitions of functions to handle string
 *
 * providing a manner to retrieve characters of a string as sequence
 * using getnc() and ungetnc() which are based on stream_string()
 */

#  include "main.h"

char stream_string(const string_t str, bool isget);
string_t file_as_string(const char *filename);
string_t stdin_as_string(void);

char ungetnc(void);
char getnc(const string_t str);

string_t reduce_string_size(string_t str);

bool clean_comments(string_t code);
bool clean_whitespaces(string_t code);
bool clean_source_code(string_t code);

#endif
