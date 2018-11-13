#ifndef _SCMIN_PAIR_H
#  define _SCMIN_PAIR_H

#  include "main.h"
#  include "gc.h"

/**
 * @brief the cons cells
 * @details containning the car and cdr which both holds
 * the head and teh rest of teh list
 */
typedef struct PAIR {
    /**
     * @brief the head of the list
     */
    sexpr_t *car;

    /**
     * @brief the rest of teh list
     */
    sexpr_t *cdr;

    /**
     * @brief `true` if this is a list and not a pair
     */
    bool islist;
} pair_t;

sexpr_t *cons(sexpr_t * car, sexpr_t * cdr);
sexpr_t *car(sexpr_t * expr);
sexpr_t *cdr(sexpr_t * expr);
void set_car(sexpr_t * expr, sexpr_t * car);
void set_cdr(sexpr_t * expr, sexpr_t * cdr);
sexpr_t *caar(sexpr_t * expr);
sexpr_t *cadr(sexpr_t * expr);
sexpr_t *cddr(sexpr_t * expr);
sexpr_t *cdar(sexpr_t * expr);
sexpr_t *caddr(sexpr_t * expr);

#endif
