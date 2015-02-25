SRCS = flavio01.cpp flavio01BlockCode.cpp

OUT = flavio01

OBJS = $(SRCS:.cpp=.o)
DEPS = $(SRCS:.cpp=.depends)

INCLUDES = -I. -I../../simulatorCore/src $(GLOBAL_INCLUDES)
#-I/usr/local/include -I/opt/local/include

#for macOSX
#LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -L/opt/local/lib -lsimMultiCores -lm -framework GLUT -framework OpenGL -L/usr/X11/lib -lboost_thread-mt -lboost_system-mt
#for linux
#LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -L/opt/local/lib -lsimMultiCores -lm -L/usr/X11/lib -lboost_thread  -lglut -lpthread

LIBS = $(GLOBAL_LIBS) -L../../simulatorCore/lib

SIMULATORLIB = ../../simulatorCore/lib/libsimMultiCores.a

CCFLAGS = $(GLOBAL_CCFLAGS)
CC = g++

.PHONY: clean all

.cpp.o:
	$(CC) $(INCLUDES) $(CCFLAGS) -c $< -o $@

%.depends: %.cpp
	$(CC) -M $(CCFLAGS) $(INCLUDES) $< > $@
	
all: $(OUT) VMEmulator

autoinstall: $(OUT) VMEmulator
	cp $(OUT) VMEmulator $(APPDIR)

$(APPDIR)/$(OUT): $(OUT)


#autoinstall: $(OUT) VMEmulator $(APPDIR)/$(OUT)
#	echo toto
#
#$(APPDIR)/$(OUT): $(OUT)
#	cp $(OUT) VMEmulator $(APPDIR)

$(OUT): $(SIMULATORLIB) $(OBJS) 
	$(CC) -o $(OUT) $(OBJS) $(LIBS)

VMEmulator: VMEmulator.cpp
	$(CC) $(CCFLAGS) $(INCLUDES) -o VMEmulator VMEmulator.cpp -L/opt/local/lib $(LIBS)

-include $(DEPS)

clean:
	rm -f *~ $(OBJS) $(OUT) $(DEPS) VMEmulator
