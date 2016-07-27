SRCS = ABCcenter.cpp ABCcenterCode.cpp

OUT = $(APPDIR)/ABCcenter

OBJS = $(SRCS:.cpp=.o)
DEPS = $(SRCS:.cpp=.depends)

INCLUDES = -I. -I../../simulatorCore/src $(GLOBAL_INCLUDES)
#-I/usr/local/include -I/opt/local/include

#for macOSX
#LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -L/opt/local/lib -lsimMultiCores -lm -framework GLUT -framework OpenGL -L/usr/X11/lib -lboost_thread-mt -lboost_system-mt
#for linux
#LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -L/opt/local/lib -lsimMultiCores -lm -L/usr/X11/lib -lboost_thread  -lglut -lpthread

LIBS = $(GLOBAL_LIBS) -L../../simulatorCore/lib -lsimCatoms2D

SIMULATORLIB = ../../simulatorCore/lib/libsimCatoms2D.a

CCFLAGS = $(GLOBAL_CCFLAGS)
CC = g++

.PHONY: clean all

.cpp.o:
	$(CC) $(INCLUDES) $(CCFLAGS) -c $< -o $@

%.depends: %.cpp
	$(CC) -M $(CCFLAGS) $(INCLUDES) $< > $@

all: $(OUT)
	@:

autoinstall: $(OUT)
	cp $(OUT)  $(APPDIR)

$(APPDIR)/$(OUT): $(OUT)


#autoinstall: $(OUT) VMEmulator $(APPDIR)/$(OUT)
#	echo toto
#
#$(APPDIR)/$(OUT): $(OUT)
#	cp $(OUT) VMEmulator $(APPDIR)

$(OUT): $(SIMULATORLIB) $(OBJS)
	$(CC) -o $(OUT) $(OBJS) $(LIBS)


ifneq ($(MAKECMDGOALS),clean)
-include $(DEPS)
endif

clean:
	rm -f *~ $(OBJS) $(OUT) $(DEPS)
