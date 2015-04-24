# Which compiler to use
CC = gcc

# Compiler flags go here.
CFLAGS = -g -std=c99 -Wall

# Linker flags go here.
LDFLAGS = -lcomedi -pthread -lm -lerl_interface -lei -lnsl

# list of sources
ELEVSRC = elev_port.c elev.c io.c erl_communication.c

# program executable file name.
TARGET = elev_port

# top-level rule, to compile everything.
all: $(TARGET)

# Define all object files.
ELEVOBJ = $(ELEVSRC:.c=.o)

# rule to link the program
$(TARGET): $(ELEVOBJ)
	$(CC) $^ simulation_elevator.a libphobos2.a -o $@ $(LDFLAGS)

# Compile: create object files from C source files.
%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@ 

# rule for cleaning re-compilable files.
clean:
	rm -f $(TARGET) $(ELEVOBJ)

rebuild:	clean all

.PHONY: all rebuild clean


#Martin Korsgaard, 2006
