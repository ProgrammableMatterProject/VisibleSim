#####################################################################
#
# --- Sample User Makefile ---
#
# GLOBAL_LIBS, GLOBAL_INCLUDES and GLOBAL_CFLAGS are set by parent Makefile
# HOWEVER: If calling make from the codeBlock directory (for more convenience to the user),
#	these variables will be empty. Hence we test their value and if undefined,
#	set them to predefined values.
#
# You will find instructions below on how to edit the Makefile to fit your needs.
#
# SRCS contains all the sources of your codeBlocks
SRCS = replay.cpp replayPlayer.cpp replayGlutContext.cpp replayWorld.cpp replayInterface.cpp replayEvent.cpp robots/smartBlocks/smartBlocksReplayWorld.cpp robots/blinkyBlocks/blinkyBlocksReplayWorld.cpp robots/slidingCubes/slidingCubesReplayWorld.cpp robots/catoms2D/catoms2DReplayWorld.cpp robots/catoms3D/catoms3DReplayWorld.cpp
#
# OUT is the output binary, where APPDIR is its enclosing directory
OUT = replay
#
# MODULELIB is the library for your target module type: e.g., -lsimBlinkyBlocks
MODULELIB = -lsimBlinkyBlocks -lsimSmartBlocks -lsimSlidingCubes -lsimCatoms2D -lsimCatoms3D -lsimOkteen -lsimDatoms -lsimNodes2D -lsimHexanodes
#
# CUSTOM_LIBS are the external dependencies of your blockcode, empty by default
CUSTOM_LIBS =
#
# End of Makefile section requiring input by user
#####################################################################

OBJS = $(SRCS:.cpp=.o)
DEPS = $(SRCS:.cpp=.depends)

OS = $(shell uname -s)
SIMULATORLIB = $(MODULELIB:-l%=../../simulatorCore/lib/lib%.a)

ifeq ($(GLOBAL_INCLUDES), )
INCLUDES = -I. -I../../simulatorCore/src -I/usr/local/include -I/opt/local/include -I/usr/X11/include
else
INCLUDES = -I. -I../../simulatorCore/src $(GLOBAL_INCLUDES)
endif

ifeq ($(GLOBAL_LIBS), )
ifeq ($(OS),Darwin)
LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -lGLEW -lglut -framework GLUT -framework OpenGL -L/usr/X11/lib /usr/local/lib/libglut.dylib /usr/local/lib/libmuparser.dylib $(MODULELIB)
else
LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -L/opt/local/lib -L/usr/X11/lib -lglut -lGL -lGLU -lGLEW -lpthread -lm -ldl -lmuparser $(MODULELIB)
endif				#OS
else
LIBS = $(GLOBAL_LIBS) -L../../simulatorCore/lib
endif				#GLOBAL_LIBS

LIBS += $(CUSTOM_LIBS)

ifeq ($(GLOBAL_CCFLAGS),)
CCFLAGS = -g -Wall -std=c++17 -Wsuggest-override -fno-stack-protector
ifeq ($(OS), Darwin)
CCFLAGS += -DGL_DO_NOT_WARN_IF_MULTI_GL_VERSION_HEADERS_INCLUDED -Wno-deprecated-declarations -Wno-overloaded-virtual
endif
else
CCFLAGS = $(GLOBAL_CCFLAGS)
endif

CC = g++

.PHONY: clean all test

.cpp.o:
	$(CC) $(INCLUDES) $(CCFLAGS) -c $< -o $@

%.depends: %.cpp
	$(CC) -M $(CCFLAGS) $(INCLUDES) $< > $@

all: $(OUT)
	@:

$(OUT): $(SIMULATORLIB) $(OBJS)
	$(CC) -o $(OUT) $(OBJS) $(LIBS)

clean:
	rm -f *~ *.o *.depends
