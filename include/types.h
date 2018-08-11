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
typedef struct KEY_VALUE kv_t;
typedef struct CONTEXT context_t;

/*! lexer types */
typedef struct TOKEN token_t;

/*! parser types */
typedef enum TYPE type_t;
typedef struct VALUE value_t;
typedef struct LAMBDA lambda_t;
typedef value_t *(*native_t) (int nargs, value_t * args);

#endif				/* _SCMIN_TYPES_H */
