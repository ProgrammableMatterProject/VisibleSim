#######################
#
# --- Tune this area to your needs ---
#
# You have to set GLOBAL_LIBS and GLOBAL_CFLAGS
# Those variables will then be sent to all the sub-makefiles
#
# two versions are proposed for each, you have to choose

OS = $(shell uname -s)

# VisibleSim local libraries
VSIM_LIBS = -lsimCatoms2D -lsimRobotBlocks -lsimMultiCores -lsimBlinkyBlocks -lsimSmartBlocks

ifeq ($(OS),Darwin)
#MacOS
GLOBAL_LIBS = "-L./ -L/usr/local/lib -L/opt/local/lib -lGLEW -lglut -framework GLUT -framework OpenGL -L/usr/X11/lib /usr/local/lib/libglut.dylib  -lboost_thread-mt  -lboost_system-mt $(VSIM_LIBS)"
OSX_CCFLAGS = -DGL_DO_NOT_WARN_IF_MULTI_GL_VERSION_HEADERS_INCLUDED
else
#Linux, Solaris, ...
GLOBAL_LIBS = "-L./ -L/usr/local/lib  -L/usr/X11/lib -lpthread -lGLEW -lGL -lGLU -lglut -lboost_thread -lboost_system $(VSIM_LIBS)"
endif

#for debug version
GLOBAL_CCFLAGS = "-g -Wall -DTINYXML_USE_STL -DTIXML_USE_STL -DDEBUG_VM_MESSAGES $(OSX_CCFLAGS)"
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
