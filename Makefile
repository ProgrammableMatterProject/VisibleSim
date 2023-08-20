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
VSIM_LIBS = -lsimCatoms3D -lsimSlidingCubes -lsimBlinkyBlocks -lsimSmartBlocks -lsimDatoms -lsimHexanodes

#for debug version
TEMP_CCFLAGS = -g -Wall -std=c++17 -DTINYXML_USE_STL -DTIXML_USE_STL -fno-stack-protector

# only use -Wsuggest-override with gcc
ifeq (g++,$(filter g++,$(CXX) $(CC), $(CPP)))
  TEMP_CCFLAGS += -Wsuggest-override
endif

# ADDITIONAL CCFLAGS
# ================
#
# MeldProcess: Uncomment this block to enable
# TEMP_CCFLAGS += -DENABLE_MELDPROCESS # Enable MeldProcess source files compilation,
#				       # required by any MeldProcess CodeBlock.
#				       # if this flag is not set, the boost libraries will not be included,
#				       # since they are the only source files using them.

# You can add any of the following debug flags to get a more verbose output
# TEMP_CCFLAGS += -DDEBUG_EVENTS #          : trace creation and destruction of all events
# TEMP_CCFLAGS += -DDEBUG_CONSUME_EVENTS #  : trace the consomption of all events
# TEMP_CCFLAGS += -DDEBUG_MESSAGES #        : traces the sending and receiving of messages
# TEMP_CCFLAGS += -DDEBUG_VM_MESSAGES #     : trace the messages sent to the multicores VM
# TEMP_CCFLAGS += -DDEBUG_OBJECT_LIFECYCLE #: trace objects construction and destruction
TEMP_CCFLAGS += -DDEBUG_GRAPHICS #        : trace graphic environment initialization and updates
# TEMP_CCFLAGS += -DDEBUG_NEIGHBORHOOD #: trace robot neighborhood updates and interface linking
TEMP_CCFLAGS += -DDEBUG_CONF_PARSING #: trace configuration file parsing
# TEMP_CCFLAGS += -DDEBUG_WORLD_LOADING #: trace world initialization
TEMP_CCFLAGS += -DDEBUG_CSG #: trace CSG parsing
TEMP_CCFLAGS += -DshowStatsFPS

#for production version
#TEMP_CCFLAGS = "-O6 -DNDEBUG -Wall -DTINYXML_USE_STL -DTIXML_USE_STL"

ifeq ($(OS),Darwin)
#MacOS (Assuming you are using the Clang compiler)
OSX_CCFLAGS = -DGL_DO_NOT_WARN_IF_MULTI_GL_VERSION_HEADERS_INCLUDED -Wno-deprecated-declarations -Wno-overloaded-virtual

GLOBAL_LIBS = "-L./ -L/usr/local/lib /usr/local/lib/libmuparser.dylib -lGLEW -lglut -framework GLUT -framework OpenGL -L/usr/X11/lib /usr/local/lib/libglut.dylib $(VSIM_LIBS) $(INC_BOOST_IF_NEEDED)"

else

#Linux, Solaris, ... (gcc)
GLOBAL_LIBS = "-L./ -L/usr/local/lib -L/usr/X11/lib $(VSIM_LIBS) -lmuparser -lglut -lGL -lGLEW -lGLU -lpthread -ldl -lstdc++ -lm $(INC_BOOST_IF_NEEDED)"

endif

GLOBAL_CCFLAGS = "$(TEMP_CCFLAGS) $(OSX_CCFLAGS)"

#
# --- End of tunable area ---
#
########################


SUBDIRS = simulatorCore/src applicationsSrc

GLOBAL_INCLUDES = "-I/usr/local/include -I/opt/local/include -I/usr/X11/include"

.PHONY: subdirs $(SUBDIRS) test doc replay
#.PHONY: subdirs $(SUBDIRS) test doc

.NOTPARALLEL: applicationsSrc/

subdirs: $(SUBDIRS)

$(SUBDIRS):
	@$(MAKE) -C $@ APPDIR=../../applicationsBin/ GLOBAL_INCLUDES=$(GLOBAL_INCLUDES) GLOBAL_LIBS=$(GLOBAL_LIBS) GLOBAL_CCFLAGS=$(GLOBAL_CCFLAGS) LOCAL_INCLUDES=$(LOCAL_INCLUDES)

test: subdirs
	@$(MAKE) -C applicationsSrc test;

replay: simulatorCore/src
	@$(MAKE) -C utilities/replay GLOBAL_INCLUDES=$(GLOBAL_INCLUDES) GLOBAL_LIBS=$(GLOBAL_LIBS) GLOBAL_CCFLAGS=$(GLOBAL_CCFLAGS) LOCAL_INCLUDES=$(LOCAL_INCLUDES);

doc:
	@$(MAKE) -C doc;

clean:
	rm -f *~ *.o
	@for dir in $(SUBDIRS); do \
	$(MAKE) -C $$dir APPDIR=../../applicationsBin clean; \
	done

realclean: clean
	$(MAKE) -C applicationsSrc APPDIR=../../applicationsBin realclean; \
	$(MAKE) -C doc clean; \
	$(MAKE) -C replay clean; \
