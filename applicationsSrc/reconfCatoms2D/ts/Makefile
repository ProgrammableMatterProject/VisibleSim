
WARNINGS = -Wall -Wextra
CCFLAGS = $(WARNINGS) -std=c++1y
LDFLAGS =
INCLUDES = 
CC = g++

SRCS = 	tuple.cpp \
	type.cpp \
	tupleSpace.cpp \
	localTupleSpace.cpp \
	CTuple.cpp \
	localCTupleSpace.cpp

OBJS = $(patsubst %.cpp,%.o,$(SRCS))

OUT = libTS.a
TARGETS = test

all: $(TARGETS)

%.o: %.cpp
	$(CC) $(CCFLAGS) $(INCLUDES) -c $< -o $@

libTS.a: $(OBJS)
	ar rcs $@ $^

test: test.o $(OUT)
	$(CC) $(CFLAGS) $(LDFLAGS) $< -o $@ -L. -lTS

.PHONY: clean realclean
clean:
	find . -name '*.o' | xargs rm -f
	find . -name '*~' | xargs rm -f

realclean: clean
	rm -f $(OUT) $(TARGETS)
