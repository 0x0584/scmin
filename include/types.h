#ifndef _SCMIN_TYPES_H
#  define _SCMIN_TYPES_H
/**
 * @file types.h
 *
 * @brief contains all types used in the project
 *
 * @details the reason why this file exists is that there's no other
 * simpler way to use functions from different files but to declaring
 * all types in one place and then defining them in the
 */

/* see gc.h */
typedef struct GC_INFO gc_info;

/* see vector.h */
typedef struct VECTOR vector_t;

/* see scope.h */
typedef struct SCOPE scope_t;
typedef struct SCOPE_BOND bond_t;

/* see lexer.h */
typedef struct TOKEN token_t;
typedef enum TOKEN_TYPE token_type;

/* see pair.h */
typedef struct PAIR pair_t;

/* see sexpr.h */
typedef double number_t;
typedef char *string_t;
typedef void *object_t;

typedef enum SYMBOLIC_EXPRESSION_TYPE type_t;
typedef struct SYMBOLIC_EXPRESSION sexpr_t;
typedef struct LAMBDA_EXPRESSION lambda_t;
typedef struct LAMBDA_NATIVE nlambda_t;

/* see eval.h */
typedef struct KEYWORD keyword_t;
typedef sexpr_t *(*k_func) (scope_t *, sexpr_t *);
typedef struct CONTEXT context_t;

#endif				/* _SCMIN_TYPES_H */
