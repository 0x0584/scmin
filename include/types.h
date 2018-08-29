#ifndef _SCMIN_TYPES_H
#  define _SCMIN_TYPES_H

typedef struct GC_INFO gc_info;

typedef double number_t;
typedef char *string_t;
typedef enum BOOLEAN bool_t;
typedef void *object_t;
typedef struct VECTOR vector_t;

typedef struct SCOPE scope_t;
typedef struct BOND bond_t;
typedef struct CONTEXT context_t;

typedef struct TOKEN token_t;
typedef enum TOKEN_TYPE token_type;

typedef enum S_EXPR_TYPE type_t;
typedef struct PAIR pair_t;
typedef struct LAMBDA lambda_t;
typedef struct NATIVE_LAMBDA Nlambda_t;
typedef struct S_EXPR sexpr_t;

typedef struct BEHAVIOR behavior_t;
#endif				/* _SCMIN_TYPES_H */
