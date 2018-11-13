# DEFAULT COMPILER SETUP
CC = gcc
# COMPILATION FLAGS
CFLAGS = -ggdb -Wall -Wextra
# -O2 -Werror -pedantic -Wpadded
# LIBRARY FLAGS
LDFLAGS = -Iinclude -lm
# DEFAULT DEBUGGING FLAGS
GDBFLAGS = -x gdb.cfg

RM = rm -f

# SOURCE OF C FILES
SRCDIR = src
# SOURCE OF HEADER FILES
DEPSDIR = include
# WHERE TO PUT OBJECT FILES
OBJDIR = bin

# GET C AND HEADER FILES FROM SRC, DEPS RECEPTIVELY
SRC := $(shell find $(SRCDIR) -name '*.c')
DEPS:= $(shell find $(DEPSDIR) -name '*.h')

# DEDUCT THE OBJECT-FILES NAME BASED ON C CORESPONDENT FILES
OBJ := $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%.o, $(SRC))

define make-dir
	for dir in $(OBJDIR); \
	do \
	mkdir -p $$dir; \
	done
endef
