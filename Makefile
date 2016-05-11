#######################
#
# --- Tune this area to your needs ---
#
# You have to set GLOBAL_LIBS and GLOBAL_CFLAGS
# Those variables will then be sent to all the sub-makefiles
#
# two versions are proposed for each, you have to choose

#for macOSX
#GLOBAL_LIBS = "-L./ -L/usr/local/lib -L/opt/local/lib -lGLEW -lglut -framework GLUT -framework OpenGL -L/usr/X11/lib /usr/local/lib/libglut.dylib -lsimMultiCores -lsimBlinkyBlocks -lsimSmartBlocks -lboost_thread-mt  -lboost_system-mt"

#for linux
GLOBAL_LIBS = "-L./ -L/usr/local/lib  -L/usr/X11/lib -lsimCatoms3D -lsimCatoms2D -lsimRobotBlocks -lsimMultiCores -lsimBlinkyBlocks -lsimSmartBlocks -lpthread -lGL -lGLEW -lGLU -lglut -lboost_thread -lboost_system"
#GLOBAL_LIBS = "-L./ -L/usr/local/lib  -L/usr/X11/lib -lpthread -lGL -lGLEW -lGLU -lglut -lboost_thread -lboost_system"
#GLOBAL_LIBS = "-L./ -L/usr/local/lib  -L/usr/X11/lib -lsimSmartBlocks -lpthread -lGL -lGLEW -lGLU -lglut -lboost_thread -lboost_system"

#for debug version
GLOBAL_CCFLAGS = "-g -Wall -DTINYXML_USE_STL -DTIXML_USE_STL -DDEBUG_VM_MESSAGES -std=c++11"

# You can add those constant definitions to get a more verbose output
# -DDEBUG_EVENTS          :  trace creation and destruction of all events
# -DDEBUG_CONSUME_EVENTS  : trace the consomption of all events
# -DDEBUG_MESSAGES        :  trace creation and destruction of all messages inside the simulator
# -DDEBUG_VM_MESSAGES     : trace the messages sent to the multicores VM

#for production version
#GLOBAL_CCFLAGS = "-O3 -DNDEBUG -Wall -DTINYXML_USE_STL -DTIXML_USE_STL"

#for TEST VERSION
#GLOBAL_CCFLAGS = "-g -Wall -DTINYXML_USE_STL -DTIXML_USE_STL -DDEBUG_VM_MESSAGES -DTEST_DETER"

#
# --- End of tunable area ---
#
########################


SUBDIRS = simulatorCore/src applicationsSrc

GLOBAL_INCLUDES = "-I/usr/local/include -I/opt/local/include -I/usr/X11/include"




.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@ APPDIR=../../applicationsBin/ GLOBAL_INCLUDES=$(GLOBAL_INCLUDES) GLOBAL_LIBS=$(GLOBAL_LIBS) GLOBAL_CCFLAGS=$(GLOBAL_CCFLAGS)

#subdirs:
#	@for dir in $(SUBDIRS); do \
#	$(MAKE) -C $$dir; \
#	done

clean:
	rm -f *~ *.o
	@for dir in $(SUBDIRS); do \
	$(MAKE) -C $$dir clean; \
	done
