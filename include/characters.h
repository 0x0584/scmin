#ifndef _SCMIN_CHARACTERS_H
#  define _SCMIN_CHARACTERS_H

/**
 * @file characters.h
 *
 * this file contains definitions of functions that would handle any
 * interaction with characters in the program, by providing a manner to
 * retrieve characters of a string as sequence using getnc() and ungetnc()
 */

#  include "main.h"

/**
 * by keeping a static pointer of the passed @p str each time, and based
 * on @p isget, it either returns the current character of in an incremental
 * order, or decrement the location by one and returning the returned character
 *
 * @param str a string
 * @param isget whether getting the current one, or push it back to the stream
 *	  again
 *
 * @return the desired character
 */
char stream_string(const string_t str, bool_t isget);

/**
 * this is literally a call to stream_string() by passing isget as false
 *
 * @param str a string
 *
 * @return the desired character
 */
char ungetnc(void);

/**
 * this is literally a call to stream_string() by passing isget as true
 *
 * @param str a string
 *
 * @return the desired character
 */
char getnc(const string_t str);

/**
 * reallocate the memory so that the size of @p str matches strlen()
 *
 * @param str a string
 *
 * @note this function modifies @p str
 */
string_t reduce_string_size(string_t str);

/**
 * takes input from the stream and format it as a string
 *
 * @param stream to get characters from
 *
 * @return the content of what was typed until the (\r || \n) as a string
 */
string_t stream_as_string(FILE *stream);

#endif
