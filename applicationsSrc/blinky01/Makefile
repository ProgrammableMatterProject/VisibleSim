SRCS = blinky01.cpp blinky01BlockCode.cpp

OUT = $(APPDIR)/blinky01

OBJS = $(SRCS:.cpp=.o)
DEPS = $(SRCS:.cpp=.depends)

INCLUDES = -I. -I../../simulatorCore/src $(GLOBAL_INCLUDES) 
#-I/usr/local/include -I/opt/local/include

#for macOSX
#LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -L/opt/local/lib -lsimMultiCores -lm -framework GLUT -framework OpenGL -L/usr/X11/lib -lboost_thread-mt -lboost_system-mt
#for linux
#LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -L/opt/local/lib -lsimMultiCores -lm -L/usr/X11/lib -lboost_thread  -lglut -lpthread

LIBS = $(GLOBAL_LIBS) -L../../simulatorCore/lib

SIMULATORLIB = ../../simulatorCore/lib/libsimBlinkyBlocks.a

CCFLAGS = $(GLOBAL_CCFLAGS)
CC = g++

.PHONY: clean all test

.cpp.o:
	$(CC) $(INCLUDES) $(CCFLAGS) -c $< -o $@

%.depends: %.cpp
	$(CC) -M $(CCFLAGS) $(INCLUDES) $< > $@

all: $(OUT)
	@:

test:
	@../../utilities/blockCodeTest.sh blinky01 $(OUT)

autoinstall: $(OUT)
	cp $(OUT) $(APPDIR)

$(APPDIR)/$(OUT): $(OUT)


$(OUT): $(SIMULATORLIB) $(OBJS) 
	$(CC) -o $(OUT) $(OBJS) $(LIBS)

ifneq ($(MAKECMDGOALS),clean)
-include $(DEPS)
endif

clean:
	rm -f *~ $(OBJS) $(OUT) $(DEPS)
