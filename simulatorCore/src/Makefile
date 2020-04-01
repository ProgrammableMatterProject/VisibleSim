# VisibleSim local libraries directory
LDIR = ../lib
# Depends files directory
DEPDIR = ../deps
# Objects files directory
OBJDIR = ../obj

OUTDIRS = $(LDIR) $(DEPDIR) $(OBJDIR)

TINYXMLSRCS = TinyXML/tinyxml.cpp TinyXML/tinyxmlerror.cpp TinyXML/tinyxmlparser.cpp
OUTDIRS += $(OBJDIR)/TinyXML $(DEPDIR)/TinyXML

MELD_DIR = meld
MELDINTERPRET_SRCS_NODIR = meldInterpretScheduler.cpp meldInterpretVM.cpp meldInterpretMessages.cpp meldInterpretEvents.cpp
MELDINTERPRET_SRCS = $(MELDINTERPRET_SRCS_NODIR:%=$(MELD_DIR)/%)

TARGETENCODING_SRCS = targetEncoding/CSG/csg.cpp targetEncoding/CSG/csgParser.cpp targetEncoding/CSG/csgUtils.cpp
OUTDIRS += $(OBJDIR)/targetEncoding/CSG $(DEPDIR)/targetEncoding/CSG

BASESIMULATOR_SRCS = $(MELDINTERPRET_SRCS) $(TINYXMLSRCS) $(TARGETENCODING_SRCS) simulator.cpp buildingBlock.cpp blockCode.cpp scheduler.cpp world.cpp network.cpp events.cpp glBlock.cpp interface.cpp openglViewer.cpp shaders.cpp vector3D.cpp matrix44.cpp color.cpp camera.cpp objLoader.cpp vertexArray.cpp trace.cpp clock.cpp qclock.cpp clockNoise.cpp configStat.cpp commandLine.cpp cppScheduler.cpp cell3DPosition.cpp configExporter.cpp lattice.cpp target.cpp statsCollector.cpp translationEvents.cpp statsIndividual.cpp random.cpp rate.cpp teleportationEvents.cpp utils.cpp

BASESIMULATOR_OBJS = $(BASESIMULATOR_SRCS:%.cpp=$(OBJDIR)/%.o)
BASESIMULATOR_DEPS = $(BASESIMULATOR_SRCS:%.cpp=$(DEPDIR)/%.depends)

BLINKYBLOCKS_DIR = blinkyBlocks
BLINKYBLOCKS_SRCS_NODIR = blinkyBlocksSimulator.cpp blinkyBlocksBlock.cpp blinkyBlocksBlockCode.cpp blinkyBlocksWorld.cpp blinkyBlocksGlBlock.cpp
BLINKYBLOCKS_SRCS = $(BLINKYBLOCKS_SRCS_NODIR:%=$(BLINKYBLOCKS_DIR)/%) $(DEBUGGER)
BLINKYBLOCKS_OBJS = $(BLINKYBLOCKS_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
BLINKYBLOCKS_DEPS = $(BLINKYBLOCKS_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

SLIDINGCUBES_DIR = slidingCubes
SLIDINGCUBES_SRCS_NODIR = slidingCubesSimulator.cpp slidingCubesBlock.cpp slidingCubesBlockCode.cpp slidingCubesWorld.cpp slidingCubesGlBlock.cpp slidingCubesMotionRules.cpp
SLIDINGCUBES_SRCS = $(SLIDINGCUBES_SRCS_NODIR:%=$(SLIDINGCUBES_DIR)/%)
SLIDINGCUBES_OBJS = $(SLIDINGCUBES_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
SLIDINGCUBES_DEPS = $(SLIDINGCUBES_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

CATOMS2D_DIR = catoms2D
CATOMS2D_SRCS_NODIR = catoms2DSimulator.cpp catoms2DBlock.cpp catoms2DBlockCode.cpp catoms2DWorld.cpp catoms2DGlBlock.cpp catoms2DRotationEvents.cpp catoms2DMotionEngine.cpp
CATOMS2D_SRCS = $(CATOMS2D_SRCS_NODIR:%=$(CATOMS2D_DIR)/%)
CATOMS2D_OBJS = $(CATOMS2D_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
CATOMS2D_DEPS = $(CATOMS2D_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

CATOMS3D_DIR = catoms3D
CATOMS3D_SRCS_NODIR = catoms3DSimulator.cpp catoms3DBlock.cpp catoms3DBlockCode.cpp catoms3DWorld.cpp catoms3DGlBlock.cpp catoms3DRotationEvents.cpp catoms3DMotionRules.cpp catoms3DMotionEngine.cpp
CATOMS3D_SRCS = $(CATOMS3D_SRCS_NODIR:%=$(CATOMS3D_DIR)/%)
CATOMS3D_OBJS = $(CATOMS3D_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
CATOMS3D_DEPS = $(CATOMS3D_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

SMARTBLOCKS_DIR = smartBlocks
SMARTBLOCKS_SRCS_NODIR = smartBlocksSimulator.cpp smartBlocksBlock.cpp smartBlocksBlockCode.cpp smartBlocksWorld.cpp smartBlocksGlBlock.cpp
SMARTBLOCKS_SRCS = $(SMARTBLOCKS_SRCS_NODIR:%=$(SMARTBLOCKS_DIR)/%)
SMARTBLOCKS_OBJS = $(SMARTBLOCKS_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
SMARTBLOCKS_DEPS = $(SMARTBLOCKS_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

OKTEEN_DIR = okteen
OKTEEN_SRCS_NODIR = okteenSimulator.cpp okteenBlock.cpp okteenBlockCode.cpp okteenWorld.cpp okteenGlBlock.cpp okteenEvents.cpp
OKTEEN_SRCS = $(OKTEEN_SRCS_NODIR:%=$(OKTEEN_DIR)/%)
OKTEEN_OBJS = $(OKTEEN_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
OKTEEN_DEPS = $(OKTEEN_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

DATOMS_DIR = datoms
DATOMS_SRCS_NODIR = datomsSimulator.cpp datomsBlock.cpp datomsBlockCode.cpp datomsWorld.cpp datomsGlBlock.cpp deformationEvents.cpp datomsMotionRules.cpp datomsMotionEngine.cpp
DATOMS_SRCS = $(DATOM_SRCS_NODIR:%=$(DATOMS_DIR)/%)
DATOMS_OBJS = $(DATOMS_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
DATOMS_DEPS = $(DATOMS_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)


NODES_DIR = nodes
NODE_SRCS_NODIR = nodeSimulator.cpp nodeBlock.cpp nodeBlockCode.cpp nodeWorld.cpp nodeGlBlock.cpp nodeMotionEngine.cpp nodeMotionEvents.cpp
NODES_SRCS = $(DATOM_SRCS_NODIR:%=$(NODES_DIR)/%)
NODES_OBJS = $(NODES_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
NODES_DEPS = $(NODES_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

HEXANODES_DIR = hexanodes
HEXANODE_SRCS_NODIR = hexanodeSimulator.cpp hexanodeBlock.cpp hexanodeBlockCode.cpp hexanodeWorld.cpp hexanodeGlBlock.cpp hexanodeMotionEngine.cpp hexanodeMotionEvents.cpp
HEXANODES_SRCS = $(DATOM_SRCS_NODIR:%=$(HEXANODES_DIR)/%)
HEXANODES_OBJS = $(HEXANODES_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
HEXANODES_DEPS = $(HEXANODES_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

SRCS = $(BASESIMULATOR_SRCS) $(BLINKYBLOCKS_SRCS) $(SMARTBLOCKS_SRCS) $(SLIDINGCUBES_SRCS) $(CATOMS2D_SRCS) $(CATOMS3D_SRCS) $(OKTEEN_SRCS) $(DATOMS_SRCS) $(NODE_SRCS) $(HEXANODE_SRCS)
OBJS = $(SRCS:%.cpp=$(OBJDIR)/%.o)

OUT = $(LDIR)/libsimSlidingCubes.a $(LDIR)/libsimBlinkyBlocks.a $(LDIR)/libsimSmartBlocks.a $(LDIR)/libsimCatoms2D.a $(LDIR)/libsimCatom3D.a $(LDIR)/libsimOkteen.a $(LDIR)/libsimDatoms.a $(LDIR)/libsimNode.a $(LDIR)/libsimHexanode.a

CORE_INC_DIRS = $(BLINKYBLOCKS_DIR) $(COREBLOCKS_DIR) $(CATOMS2D_DIR) $(CATOMS3D_DIR) $(SMARTBLOCKS_DIR) $(OKTEEN_DIR) $(DATOMS_DIR) $(MELD_DIR)
INCLUDES = $(GLOBAL_INCLUDES) -I. $(CORE_INC_DIRS:%=-I%)

OUTDIRS += $(CORE_INC_DIRS:%=$(DEPDIR)/%) $(CORE_INC_DIRS:%=$(OBJDIR)/%)

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
	$(CC) $(CCFLAGS) $(DEPFLAGS) $(INCLUDES) -c $< -o $@
	$(POSTCOMPILE)


all: $(LDIR)/libsimBlinkyBlocks.a $(LDIR)/libsimSmartBlocks.a $(LDIR)/libsimSlidingCubes.a $(LDIR)/libsimCatoms2D.a $(LDIR)/libsimCatoms3D.a $(LDIR)/libsimOkteen.a $(LDIR)/libsimDatoms.a $(LDIR)/libsimNode.a $(LDIR)/libsimHexanode.a

${OUTDIRS}:
	@mkdir -p $@

$(LDIR)/libsimBlinkyBlocks.a: $(BLINKYBLOCKS_OBJS)
	@echo "Archiving BlinkyBlocks library..."
	@ar rcs $(LDIR)/libsimBlinkyBlocks.a $(BLINKYBLOCKS_OBJS)

$(LDIR)/libsimSlidingCubes.a: $(SLIDINGCUBES_OBJS)
	@echo "Archiving SlidingCubes library..."
	@ar rcs $(LDIR)/libsimSlidingCubes.a $(SLIDINGCUBES_OBJS)

$(LDIR)/libsimCatoms2D.a: $(CATOMS2D_OBJS)
	@echo "Archiving Catoms2D library..."
	@ar rcs $(LDIR)/libsimCatoms2D.a $(CATOMS2D_OBJS)

$(LDIR)/libsimCatoms3D.a: $(CATOMS3D_OBJS)
	@echo "Archiving Catoms3D library..."
	@ar rcs $(LDIR)/libsimCatoms3D.a $(CATOMS3D_OBJS)

$(LDIR)/libsimSmartBlocks.a: $(SMARTBLOCKS_OBJS)
	@echo "Archiving SmartBlocks library..."
	@ar rcs $(LDIR)/libsimSmartBlocks.a $(SMARTBLOCKS_OBJS)

$(LDIR)/libsimOkteen.a: $(OKTEEN_OBJS)
	@echo "Archiving Okteen library..."
	@ar rcs $(LDIR)/libsimOkteen.a $(OKTEEN_OBJS)

$(LDIR)/libsimDatoms.a: $(DATOMS_OBJS)
	@echo "Archiving Datoms library..."
	@ar rcs $(LDIR)/libsimDatoms.a $(DATOMS_OBJS)

$(LDIR)/libsimNode.a: $(NODE_OBJS)
	@echo "Archiving Node library..."
	@ar rcs $(LDIR)/libsimNode.a $(NODE_OBJS)

$(LDIR)/libsimHexanode.a: $(HEXANODE_OBJS)
	@echo "Archiving Hexanode library..."
	@ar rcs $(LDIR)/libsimHexanode.a $(HEXANODE_OBJS)

$(DEPDIR)/%.depends: ;
.PRECIOUS: $(DEPDIR)/%.depends

-include $(patsubst %,$(DEPDIR)/%.depends,$(basename $(SRCS)))

clean:
	rm -f *~ $(OBJS) $(OUT) $(DEPS) ../include/*.h
	rm -rf $(OUTDIRS)
