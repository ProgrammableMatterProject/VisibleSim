SRCS = angle.cpp map.cpp segment.cpp catom2D1.cpp catom2D1BlockCode.cpp

OUT = catom2D1

OBJS = $(SRCS:.cpp=.o)
DEPS = $(SRCS:.cpp=.depends)

INCLUDES = -I. -I../../simulatorCore/src $(GLOBAL_INCLUDES) -I./ts
#-I/usr/local/include -I/opt/local/include

#for macOSX
#LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -L/opt/local/lib -lsimMultiCores -lm -framework GLUT -framework OpenGL -L/usr/X11/lib -lboost_thread-mt -lboost_system-mt
#for linux
#LIBS = -L./ -L../../simulatorCore/lib -L/usr/local/lib -L/opt/local/lib -lsimMultiCores -lm -L/usr/X11/lib -lboost_thread  -lglut -lpthread

LIBS = $(GLOBAL_LIBS) -L../../simulatorCore/lib -L./ts -lTS

SIMULATORLIB = ../../simulatorCore/lib/libsimCatoms2D.a ts/libTS.a

CCFLAGS = $(GLOBAL_CCFLAGS) -std=c++1y
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


#autoinstall: $(OUT) VMEmulator $(APPDIR)/$(OUT)
#	echo toto
#
#$(APPDIR)/$(OUT): $(OUT)
#	cp $(OUT) VMEmulator $(APPDIR)

$(OUT): $(SIMULATORLIB) $(OBJS) 
	$(CC) -o $(OUT) $(OBJS) $(LIBS)


-include $(DEPS)

clean:
	rm -f *~ $(OBJS) $(OUT) $(DEPS)
