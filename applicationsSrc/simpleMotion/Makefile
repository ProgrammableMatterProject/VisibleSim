# Get current directory's name
mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(notdir $(patsubst %/,%,$(dir $(mkfile_path))))
APPDIR = ../../applicationsBin/$(current_dir)

SRCS = simpleMotion.cpp  simpleMotionCode.cpp

OUT = $(APPDIR)/simpleMotion

OBJS = $(SRCS:.cpp=.o)
DEPS = $(SRCS:.cpp=.depends)

INCLUDES = -I. -I../../simulatorCore/src $(GLOBAL_INCLUDES)

LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -L/opt/local/lib -lsimSmartBlocks -lm -L/usr/X11/lib  -lmuparser -lglut -lGL -lGLU -lGLEW -lpthread $(MODULELIB)
SIMULATORLIB = ../../simulatorCore/lib/libsimSmartBlocks.a

CCFLAGS = $(GLOBAL_CCFLAGS)
CC = g++

.PHONY: clean all

.cpp.o:
	$(CC) $(INCLUDES) $(CCFLAGS) -c $< -o $@

%.depends: %.cpp
	$(CC) -M $(CCFLAGS) $(INCLUDES) $< > $@

all: $(OUT)

autoinstall: $(OUT)
	cp $(OUT)  $(APPDIR)

$(APPDIR)/$(OUT): $(OUT)

$(OUT): $(SIMULATORLIB) $(OBJS)
	$(CC) -o $(OUT) $(OBJS) $(LIBS)

-include $(DEPS)

clean:
	rm -f *~ $(OBJS) $(OUT) $(DEPS)
