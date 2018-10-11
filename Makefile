include init.mk

# DEFAULT PROGRAM NAME
EXEC = scmin

# DEFAULT PROGRAM ARGUMENTS
ARGS = "(+ 4 5)"

all: main
	@echo  "done.\n"

main: build-dir $(OBJ)
	@echo  "\nbuilding executable.."
	$(CC) $(OBJ) -o $(EXEC) $(LDFLAGS)
	@echo  ""

build-dir:
	@echo "building objects.."
	@$(call make-dir)

$(OBJDIR)/%.o: $(SRCDIR)/%.c $(DEPS)
	$(CC) $(CFLAGS) -c -o $@ $< $(LDFLAGS) 

run: build
	@echo "Passed Arguments:\t \033[0;32m" $(ARGS)
	@echo "\033[0m"
	./$(EXEC) $(ARGS)
	@echo " "

build: clean all

valg: build
	valgrind ./$(EXEC)

valk: build
	valkyrie ./$(EXEC)

gdb: build
	gdb $(GDBFLAGS) $(EXEC)
clean:
	@echo  "cleaning up.."
	$(RM) $(EXEC)
	$(RM) $(OBJ)
	@echo  ""
