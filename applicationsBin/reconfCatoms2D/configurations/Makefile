CC = g++
CFLAGS = -Wall -std=c++11
EXEC_NAME = tgaToVisibleSim
INCLUDES =
LIBS =
OBJ_FILES = arguments.o pixel.o targa.o tgaToVisibleSim.o

all : $(EXEC_NAME)

$(EXEC_NAME) : $(OBJ_FILES)
	$(CC) -o $(EXEC_NAME) $(OBJ_FILES) $(LIBS)

%.o: %.cpp
	$(CC) $(CFLAGS) $(INCLUDES) -o $@ -c $<

clean :
	rm $(EXEC_NAME) $(OBJ_FILES)
