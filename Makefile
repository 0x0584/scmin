# DEFAULT COMPILER SETUP
CC = gcc
CFLAGS = -ggdb -O2 -Wall -Wextra #-Werror
# -pedantic -Wpadded
LDFLAGS = -I. -lm

# DEFAULT PROGRAM NAME
EXEC = scmin
# DEFAULT PROGRAM ARGUMENTS
ARGS = 9 2

# SOURCE OF C FILES
SRCDIR = src
# SOURCE OF HEADER FILES
DEPSDIR = include
# WHERE TO PUT OBJECT FILES 
OBJDIR = bin

# GET C AND HEADER FILES INTO SRC, DEPS RECEPTIVELY
SRC := $(shell find $(SRCDIR) -name '*.c')
DEPS:= $(shell find $(DEPSDIR) -name '*.h')

# DEDUCT THE OBJECT-FILE NAMES BASED ON C FILES
OBJ := $(patsubst $(SRCDIR)/%.c, $(OBJDIR)/%.o, $(SRC))

all: main
	@echo  "done.\n"

main: build-dir $(OBJ)
	@echo  "\nbuilding executable.."
	$(CC) $(OBJ) -o $(EXEC) $(LDFLAGS)
	@echo  ""

build-dir:
	@echo "building objects.."
	@$(call make-dir)

define make-dir
	for dir in $(OBJDIR); \
	do \
	mkdir -p $$dir; \
	done
endef

$(OBJDIR)/%.o: $(SRCDIR)/%.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $< $(LDFLAGS) 

run: all
	@echo "Passed Arguments:\t \033[0;32m" $(ARGS)
	@echo "\033[0m"
	./$(EXEC) $(ARGS)
	@echo " "

build:clean all

clean:
	@echo  "cleaning up.."
	$(RM) $(EXEC)
	$(RM) $(OBJ)
	@echo  ""
