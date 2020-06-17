# VisibleSim local libraries directory
LDIR = ../lib
# Depends files directory
DEPDIR = ../deps
# Objects files directory
OBJDIR = ../obj

OUTDIRS = $(LDIR) $(DEPDIR) $(OBJDIR)

TINYXMLSRCS = deps/TinyXML/tinyxml.cpp deps/TinyXML/tinyxmlerror.cpp deps/TinyXML/tinyxmlparser.cpp
OUTDIRS += $(OBJDIR)/deps/TinyXML $(DEPDIR)/deps/TinyXML

MELD_DIR = meld
MELDINTERPRET_SRCS_NODIR = meldInterpretScheduler.cpp meldInterpretVM.cpp meldInterpretMessages.cpp meldInterpretEvents.cpp
MELDINTERPRET_SRCS = $(MELDINTERPRET_SRCS_NODIR:%=$(MELD_DIR)/%)

TARGETENCODING_SRCS = csg/csg.cpp csg/csgParser.cpp # csg/csgUtils.cpp
OUTDIRS += $(OBJDIR)/csg $(DEPDIR)/csg

BASESIMULATOR_SRCS = $(MELDINTERPRET_SRCS) $(TINYXMLSRCS) $(TARGETENCODING_SRCS) base/simulator.cpp base/buildingBlock.cpp base/blockCode.cpp events/scheduler.cpp base/world.cpp comm/network.cpp events/events.cpp base/glBlock.cpp gui/interface.cpp gui/openglViewer.cpp gui/shaders.cpp math/vector3D.cpp math/matrix44.cpp utils/color.cpp gui/camera.cpp gui/objLoader.cpp gui/vertexArray.cpp utils/trace.cpp clock/clock.cpp clock/qclock.cpp clock/clockNoise.cpp stats/configStat.cpp utils/commandLine.cpp events/cppScheduler.cpp grid/cell3DPosition.cpp utils/configExporter.cpp grid/lattice.cpp grid/target.cpp stats/statsCollector.cpp motion/translationEvents.cpp stats/statsIndividual.cpp utils/random.cpp comm/rate.cpp motion/teleportationEvents.cpp utils/utils.cpp replay/replayExporter.cpp replay/replayPlayer.cpp

BASESIMULATOR_OBJS = $(BASESIMULATOR_SRCS:%.cpp=$(OBJDIR)/%.o)
BASESIMULATOR_DEPS = $(BASESIMULATOR_SRCS:%.cpp=$(DEPDIR)/%.depends)

BLINKYBLOCKS_DIR = robots/blinkyBlocks
BLINKYBLOCKS_SRCS_NODIR = blinkyBlocksSimulator.cpp blinkyBlocksBlock.cpp blinkyBlocksBlockCode.cpp blinkyBlocksWorld.cpp blinkyBlocksGlBlock.cpp
BLINKYBLOCKS_SRCS = $(BLINKYBLOCKS_SRCS_NODIR:%=$(BLINKYBLOCKS_DIR)/%) $(DEBUGGER)
BLINKYBLOCKS_OBJS = $(BLINKYBLOCKS_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
BLINKYBLOCKS_DEPS = $(BLINKYBLOCKS_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

SLIDINGCUBES_DIR = robots/slidingCubes
SLIDINGCUBES_SRCS_NODIR = slidingCubesSimulator.cpp slidingCubesBlock.cpp slidingCubesBlockCode.cpp slidingCubesWorld.cpp slidingCubesGlBlock.cpp slidingCubesMotionRules.cpp
SLIDINGCUBES_SRCS = $(SLIDINGCUBES_SRCS_NODIR:%=$(SLIDINGCUBES_DIR)/%)
SLIDINGCUBES_OBJS = $(SLIDINGCUBES_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
SLIDINGCUBES_DEPS = $(SLIDINGCUBES_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

CATOMS2D_DIR = robots/catoms2D
CATOMS2D_SRCS_NODIR = catoms2DSimulator.cpp catoms2DBlock.cpp catoms2DBlockCode.cpp catoms2DWorld.cpp catoms2DGlBlock.cpp catoms2DRotationEvents.cpp catoms2DMotionEngine.cpp
CATOMS2D_SRCS = $(CATOMS2D_SRCS_NODIR:%=$(CATOMS2D_DIR)/%)
CATOMS2D_OBJS = $(CATOMS2D_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
CATOMS2D_DEPS = $(CATOMS2D_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

CATOMS3D_DIR = robots/catoms3D
CATOMS3D_SRCS_NODIR = catoms3DSimulator.cpp catoms3DBlock.cpp catoms3DBlockCode.cpp catoms3DWorld.cpp catoms3DGlBlock.cpp catoms3DRotationEvents.cpp catoms3DMotionRules.cpp catoms3DMotionEngine.cpp
CATOMS3D_SRCS = $(CATOMS3D_SRCS_NODIR:%=$(CATOMS3D_DIR)/%)
CATOMS3D_OBJS = $(CATOMS3D_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
CATOMS3D_DEPS = $(CATOMS3D_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

SMARTBLOCKS_DIR = robots/smartBlocks
SMARTBLOCKS_SRCS_NODIR = smartBlocksSimulator.cpp smartBlocksBlock.cpp smartBlocksBlockCode.cpp smartBlocksWorld.cpp smartBlocksGlBlock.cpp
SMARTBLOCKS_SRCS = $(SMARTBLOCKS_SRCS_NODIR:%=$(SMARTBLOCKS_DIR)/%)
SMARTBLOCKS_OBJS = $(SMARTBLOCKS_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
SMARTBLOCKS_DEPS = $(SMARTBLOCKS_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

OKTEEN_DIR = robots/okteen
OKTEEN_SRCS_NODIR = okteenSimulator.cpp okteenBlock.cpp okteenBlockCode.cpp okteenWorld.cpp okteenGlBlock.cpp okteenEvents.cpp
OKTEEN_SRCS = $(OKTEEN_SRCS_NODIR:%=$(OKTEEN_DIR)/%)
OKTEEN_OBJS = $(OKTEEN_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
OKTEEN_DEPS = $(OKTEEN_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

DATOMS_DIR = robots/datoms
DATOMS_SRCS_NODIR = datomsSimulator.cpp datomsBlock.cpp datomsBlockCode.cpp datomsWorld.cpp datomsGlBlock.cpp deformationEvents.cpp datomsMotionRules.cpp datomsMotionEngine.cpp
DATOMS_SRCS = $(DATOMS_SRCS_NODIR:%=$(DATOMS_DIR)/%)
DATOMS_OBJS = $(DATOMS_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
DATOMS_DEPS = $(DATOMS_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

NODES2D_DIR = robots/nodes2D
NODES2D_SRCS_NODIR = nodes2DSimulator.cpp nodes2DBlock.cpp nodes2DBlockCode.cpp nodes2DWorld.cpp nodes2DGlBlock.cpp nodes2DMotionEngine.cpp nodes2DMotionEvents.cpp
NODES2D_SRCS = $(NODES2D_SRCS_NODIR:%=$(NODES2D_DIR)/%)
NODES2D_OBJS = $(NODES2D_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
NODES2D_DEPS = $(NODES2D_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

HEXANODES_DIR = robots/hexanodes
HEXANODES_SRCS_NODIR = hexanodesSimulator.cpp hexanodesBlock.cpp hexanodesBlockCode.cpp hexanodesWorld.cpp hexanodesGlBlock.cpp hexanodesMotionEngine.cpp hexanodesMotionEvents.cpp
HEXANODES_SRCS = $(HEXANODES_SRCS_NODIR:%=$(HEXANODES_DIR)/%)
HEXANODES_OBJS = $(HEXANODES_SRCS:%.cpp=$(OBJDIR)/%.o) $(BASESIMULATOR_OBJS)
HEXANODES_DEPS = $(HEXANODES_SRCS:%.cpp=$(DEPDIR)/%.depends) $(BASESIMULATOR_DEPS)

SRCS = $(BASESIMULATOR_SRCS) $(BLINKYBLOCKS_SRCS) $(SMARTBLOCKS_SRCS) $(SLIDINGCUBES_SRCS) $(CATOMS2D_SRCS) $(CATOMS3D_SRCS) $(OKTEEN_SRCS) $(DATOMS_SRCS) $(NODES2D_SRCS) $(HEXANODES_SRCS)
OBJS = $(SRCS:%.cpp=$(OBJDIR)/%.o)

OUT = $(LDIR)/libsimSlidingCubes.a $(LDIR)/libsimBlinkyBlocks.a $(LDIR)/libsimSmartBlocks.a $(LDIR)/libsimCatoms2D.a $(LDIR)/libsimCatom3D.a $(LDIR)/libsimOkteen.a $(LDIR)/libsimDatoms.a $(LDIR)/libsimNodes2D.a $(LDIR)/libsimHexanodes.a

CORE_DIRS = base/ clock/ comm/ deps/ grid/ gui/ math/ meld/ motion/ stats/ robots/ events/ utils/ replay/
CORE_INC_DIRS = $(CORE_DIRS) $(BLINKYBLOCKS_DIR) $(SLIDINGCUBES_DIR) $(CATOMS2D_DIR) $(CATOMS3D_DIR) $(SMARTBLOCKS_DIR) $(OKTEEN_DIR) $(DATOMS_DIR) $(NODES2D_DIR) $(HEXANODES_DIR) $(MELD_DIR)

OUTDIRS += $(CORE_INC_DIRS:%=$(DEPDIR)/%) $(CORE_INC_DIRS:%=$(OBJDIR)/%)
INCLUDES = $(GLOBAL_INCLUDES) -I. # $(CORE_INC_DIRS:%=-I%)

#for linux
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


all: $(LDIR)/libsimBlinkyBlocks.a $(LDIR)/libsimSmartBlocks.a $(LDIR)/libsimSlidingCubes.a $(LDIR)/libsimCatoms2D.a $(LDIR)/libsimCatoms3D.a $(LDIR)/libsimOkteen.a $(LDIR)/libsimDatoms.a $(LDIR)/libsimNodes2D.a $(LDIR)/libsimHexanodes.a

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

$(LDIR)/libsimNodes2D.a: $(NODES2D_OBJS)
	@echo "Archiving Nodes2D library..."
	@ar rcs $(LDIR)/libsimNodes2D.a $(NODES2D_OBJS)

$(LDIR)/libsimHexanodes.a: $(HEXANODES_OBJS)
	@echo "Archiving Hexanodes library..."
	@ar rcs $(LDIR)/libsimHexanodes.a $(HEXANODES_OBJS)

$(DEPDIR)/%.depends: ;
.PRECIOUS: $(DEPDIR)/%.depends

-include $(patsubst %,$(DEPDIR)/%.depends,$(basename $(SRCS)))

clean:
	rm -f *~ $(OBJS) $(OUT) $(DEPS) ../include/*.h
	rm -rf $(OUTDIRS)
