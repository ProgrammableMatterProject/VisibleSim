# VisibleSim local libraries directory
LDIR = ../lib
# Depends files directory
DEPDIR = ../deps
# Objects files directory
OBJDIR = ../obj

OUTDIRS = $(LDIR) $(DEPDIR) $(OBJDIR)

TINYXMLSRCS = TinyXML/tinyxml.cpp TinyXML/tinyxmlerror.cpp TinyXML/tinyxmlparser.cpp
OUTDIRS += $(OBJDIR)/TinyXML $(DEPDIR)/TinyXML

MELDDEBUGGER_SRCS = Debugger/debug_Simhandler.cpp Debugger/debug_Simprompt.cpp Debugger/types.cpp
# Note : when using TIXML_USE_STL flag, TinyXML/tinystr.cpp produce no code, so I removed it from the source list as ar was complaining
OUTDIRS += $(OBJDIR)/Debugger $(DEPDIR)/Debugger

MELDPROCESS_SRCS = $(MELDDEBUGGER_SRCS) meldProcessScheduler.cpp meldProcessVM.cpp meldProcessVMCommands.cpp meldProcessDebugger.cpp meldProcessEvents.cpp

MELDINTERPRET_SRCS = meldInterpretScheduler.cpp meldInterpretVM.cpp meldInterpretMessages.cpp meldInterpretEvents.cpp

BASESIMULATOR_SRCS = $(MELDPROCESS_SRCS) $(MELDINTERPRET_SRCS) $(TINYXMLSRCS) simulator.cpp buildingBlock.cpp blockCode.cpp scheduler.cpp world.cpp network.cpp events.cpp glBlock.cpp interface.cpp openglViewer.cpp shaders.cpp vector3D.cpp matrix44.cpp color.cpp camera.cpp objLoader.cpp vertexArray.cpp trace.cpp clock.cpp configStat.cpp commandLine.cpp cppScheduler.cpp cell3DPosition.cpp configExporter.cpp lattice.cpp capabilities.cpp

BASESIMULATOR_OBJS = $(BASESIMULATOR_SRCS:%.cpp=$(OBJDIR)/%.o)
BASESIMULATOR_DEPS = $(BASESIMULATOR_SRCS:%.cpp=$(DEPDIR)/%.depends)

BLINKYBLOCKS_SRCS = $(DEBUGGER) blinkyBlocksSimulator.cpp blinkyBlocksBlock.cpp blinkyBlocksBlockCode.cpp blinkyBlocksWorld.cpp blinkyBlocksGlBlock.cpp blinkyBlocksGeneric.cpp
BLINKYBLOCKS_OBJS = $(BLINKYBLOCKS_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
BLINKYBLOCKS_DEPS = $(BLINKYBLOCKS_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

ROBOTBLOCKS_SRCS = robotBlocksSimulator.cpp robotBlocksBlock.cpp robotBlocksBlockCode.cpp robotBlocksWorld.cpp robotBlocksGlBlock.cpp robotBlocksEvents.cpp robotBlocksGeneric.cpp
ROBOTBLOCKS_OBJS = $(ROBOTBLOCKS_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
ROBOTBLOCKS_DEPS = $(ROBOTBLOCKS_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

CATOMS2D_SRCS = catoms2DSimulator.cpp catoms2DBlock.cpp catoms2DBlockCode.cpp catoms2DWorld.cpp catoms2DGlBlock.cpp catoms2DEvents.cpp catoms2DMove.cpp catoms2DGeneric.cpp
CATOMS2D_OBJS = $(CATOMS2D_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
CATOMS2D_DEPS = $(CATOMS2D_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

CATOMS3D_SRCS = catoms3DSimulator.cpp catoms3DBlock.cpp catoms3DBlockCode.cpp catoms3DWorld.cpp catoms3DGlBlock.cpp catoms3DEvents.cpp skeleton.cpp catoms3DGeneric.cpp
CATOMS3D_OBJS = $(CATOMS3D_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
CATOMS3D_DEPS = $(CATOMS3D_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

SMARTBLOCKS_SRCS = smartBlocksSimulator.cpp smartBlocksBlock.cpp smartBlocksCapabilities.cpp smartBlocksBlockCode.cpp smartBlocksWorld.cpp smartBlocksGlBlock.cpp smartBlocksEvents.cpp smartBlocksGeneric.cpp
SMARTBLOCKS_OBJS = $(SMARTBLOCKS_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
SMARTBLOCKS_DEPS = $(SMARTBLOCKS_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

SRCS = $(BASESIMULATOR_SRCS) $(BLINKYBLOCKS_SRCS) $(SMARTBLOCKS_SRCS) $(ROBOTBLOCKS_SRCS) $(CATOMS2D_SRCS) $(CATOMS3D_SRCS)
OBJS = $(SRCS:%.cpp=$(OBJDIR)/%.o)

OUT = $(LDIR)/libsimRobotBlocks.a $(LDIR)/libsimBlinkyBlocks.a $(LDIR)/libsimSmartBlocks.a $(LDIR)/libsimCatoms2D.a $(LDIR)/libsimCatom3D.a

INCLUDES = $(GLOBAL_INCLUDES) -I.

#for macOSX
#LIBS = -L./ -L/usr/local/lib -framework GLUT -framework OpenGL -L/usr/X11/lib /usr/local/lib/libglut.dylib -lboost_thread-mt

#for linux
#LIBS = -L./ -L/usr/local/lib  -L/usr/X11/lib -lboost_thread-mt
LIBS = $(GLOBAL_LIBS)

#for debug version
#CCFLAGS = -g -Wall -DTINYXML_USE_STL -DTIXML_USE_STL

#for production version
#CCFLAGS = -O2 -NDEBUG -Wall -DTINYXML_USE_STL -DTIXML_USE_STL

CCFLAGS = $(GLOBAL_CCFLAGS)
DEPFLAGS = -MT $@ -MMD -MP -MF $(DEPDIR)/$*.Td
POSTCOMPILE = @mv -f $(DEPDIR)/$*.Td $(DEPDIR)/$*.depends
CC = g++

# Create compilation directories if they don't exist
$(shell mkdir -p $(OUTDIRS) >/dev/null)

.PHONY: clean all

$(OBJDIR)/%.o: %.cpp
	@echo "-> Compiling $@"
	@$(CC) $(CCFLAGS) $(DEPFLAGS) $(INCLUDES) -c $< -o $@
	$(POSTCOMPILE)


all: $(LDIR)/libsimBlinkyBlocks.a $(LDIR)/libsimSmartBlocks.a $(LDIR)/libsimRobotBlocks.a $(LDIR)/libsimCatoms2D.a $(LDIR)/libsimCatoms3D.a

${OUTDIRS}:
	@mkdir -p $@

$(LDIR)/libsimBlinkyBlocks.a: $(BLINKYBLOCKS_OBJS)
	@echo "Archiving BlinkyBlocks library..."
	@ar rcs $(LDIR)/libsimBlinkyBlocks.a $(BLINKYBLOCKS_OBJS)

$(LDIR)/libsimRobotBlocks.a: $(ROBOTBLOCKS_OBJS)
	@echo "Archiving RobotBlocks library..."
	@ar rcs $(LDIR)/libsimRobotBlocks.a $(ROBOTBLOCKS_OBJS)

$(LDIR)/libsimCatoms2D.a: $(CATOMS2D_OBJS)
	@echo "Archiving Catoms2D library..."
	@ar rcs $(LDIR)/libsimCatoms2D.a $(CATOMS2D_OBJS)

$(LDIR)/libsimCatoms3D.a: $(CATOMS3D_OBJS)
	@echo "Archiving Catoms3D library..."
	@ar rcs $(LDIR)/libsimCatoms3D.a $(CATOMS3D_OBJS)

$(LDIR)/libsimSmartBlocks.a: $(SMARTBLOCKS_OBJS)
	@echo "Archiving SmartBlocks library..."
	@ar rcs $(LDIR)/libsimSmartBlocks.a $(SMARTBLOCKS_OBJS)

$(DEPDIR)/%.depends: ;
.PRECIOUS: $(DEPDIR)/%.depends

-include $(patsubst %,$(DEPDIR)/%.depends,$(basename $(SRCS)))

clean:
	rm -f *~ $(OBJS) $(OUT) $(DEPS) ../include/*.h
	rm -rf $(OUTDIRS)
