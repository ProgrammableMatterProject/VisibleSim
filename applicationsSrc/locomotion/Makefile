SRCS = locomotion.cpp  locomotionCode.cpp

OUT = $(APPDIR)/locomotion

OBJS = $(SRCS:.cpp=.o)
DEPS = $(SRCS:.cpp=.depends)

INCLUDES = -I. -I../../simulatorCore/src $(GLOBAL_INCLUDES)

LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -L/opt/local/lib -lsimRobotBlocks -lm -L/usr/X11/lib -lmuparser -lglut -lGL -lGLU -lGLEW -lpthread
SIMULATORLIB = ../../simulatorCore/lib/libsimRobotBlocks.a

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
