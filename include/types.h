#ifndef _SCMIN_TYPES_H
#  define _SCMIN_TYPES_H

/*! GC information */
typedef struct GC_INFO gc_info;

/*! basic types */
typedef double number_t;
typedef char *string_t;
typedef enum BOOLEAN bool_t;
typedef void *object_t;
typedef struct VECTOR vector_t;

/*! repl types */
typedef struct SCOPE scope_t;
typedef struct DATA data_t;
typedef struct CONTEXT context_t;

/*! lexer types */
typedef struct TOKEN token_t;
typedef enum TOKEN_TYPE token_type;

/*! parser types */
typedef enum TYPE type_t;
typedef struct PAIR pair_t;
typedef struct S_EXPR sexpr_t;
typedef struct LAMBDA lambda_t;
typedef sexpr_t *(*native_t) (int nargs, sexpr_t * args);

#endif				/* _SCMIN_TYPES_H */
