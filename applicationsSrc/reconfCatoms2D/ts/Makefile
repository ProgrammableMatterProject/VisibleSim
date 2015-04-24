
WARNINGS = -Wall -Wextra
CCFLAGS = $(WARNINGS)
LDFLAGS =
INCLUDES = 
CC = g++

SRCS = tuple.cpp \
	tupleSpace.cpp \
	localTupleSpace.cpp

OBJS = $(patsubst %.cpp,%.o,$(SRCS))

OUT = libTS.a
TARGETS = test

all: $(TARGETS)

%.o: %.cpp
	$(CC) $(CCFLAGS) $(INCLUDES) -c $< -o $@

libTS.a: $(OBJS)
	ar rcs $@ $^

test: $(OUT) test.o
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

.PHONY: clean realclean
clean:
	find . -name '*.o' | xargs rm -f
	find . -name '*~' | xargs rm -f

realclean: clean
	rm -f $(OUT) $(TARGETS)
