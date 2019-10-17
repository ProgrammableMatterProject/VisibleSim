/**
 * @file   coatingBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Oct  9 17:11:51 2019
 *
 * @brief
 *
 *
 */


#ifndef COATING_BLOCKCODE_H_
#define COATING_BLOCKCODE_H_

#include <deque>
#include <unordered_set>
#include <set>

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DMotionRules.h"
#include "rotation3DEvents.h"
#include "catoms3DBlock.h"
#include "cell3DPosition.h"

#include "coatingMessages.hpp"
#include "coatingRuleMatcher.hpp"
#include "coatingUtils.hpp"

#define IT_MODULE_INSERTION 1


class CoatingBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
    inline static const int B = 6;
    inline static int X_MAX = numeric_limits<int>::min();
    inline static int Y_MAX = numeric_limits<int>::min();
    inline static int Z_MAX = numeric_limits<int>::min();
    inline static int X_MIN = numeric_limits<int>::max();
    inline static int Y_MIN = numeric_limits<int>::max();
    inline static int Z_MIN = numeric_limits<int>::max();
    inline static Cell3DPosition scaffoldSeedPos = Cell3DPosition(-1, -1, -1);
    inline static const Cell3DPosition& sbSeedPos = Cell3DPosition(3, 3, 3);

    inline static Time t0 = 0;
    inline static bool BUILDING_MODE = false; // const after call to parseUserCommandLineArgument
    inline static bool HIGHLIGHT_CSG = false;
    inline static bool HIGHLIGHT_SCAFFOLD = false;
    inline static bool HIGHLIGHT_COATING = false;
    inline static bool HIGHLIGHT_RES = false;
    inline static int HIGHLIGHT_layer = -1;
    // inline static bool PYRAMID_MODE = false;
    inline static bool sandboxInitialized;

    bool highlightedReachablePos = false;

    // BlockCode
    Scheduler *scheduler;
    World *world;
    Lattice *lattice;
    Catoms3D::Catoms3DBlock *catom;
    CoatingRuleMatcher *rm;

    CoatingBlockCode(Catoms3D::Catoms3DBlock *host);
    ~CoatingBlockCode();

    /**
     * \brief Global message handler for this instance of the blockcode
     * \param msg Message received b
     y the module
     * \param sender Connector that has received the message and hence that is connected to the sender */
    void processReceivedMessage(MessagePtr msg, P2PNetworkInterface* sender);

    void startup() override;
    void processLocalEvent(EventPtr pev) override;
    void onBlockSelected() override;
    void onAssertTriggered() override;

    bool parseUserCommandLineArgument(int& argc, char **argv[]) override;

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new CoatingBlockCode((Catoms3DBlock*)host));
    }

    // Scaffolding
    BranchIndex branch;
    AgentRole role;
    Cell3DPosition coordinatorPos;
    Cell3DPosition targetPosition;
    bool rotating = false;
    bool initialized = false; //!< Indicates whether this module has called startup() yet

    static const Cell3DPosition norm(const Cell3DPosition& pos);
    static const Cell3DPosition denorm(const Cell3DPosition& pos);
    void initializeSandbox();
    bool isInsideCSGFn(const Cell3DPosition& pos) const;
    void scheduleRotationTo(const Cell3DPosition& pos, Catoms3DBlock* pivot = NULL);
    void highlightCSGScaffold(bool force = false);
    Cell3DPosition determineScaffoldSeedPosition();
    Cell3DPosition getTileRootPosition(const Cell3DPosition& pos) const;

    // Motion coordination
#define SET_GREEN_LIGHT(x) setGreenLight(x, __LINE__)
    void setGreenLight(bool onoff, int _line_);
    bool isAdjacentToPosition(const Cell3DPosition& pos) const;
    inline static Time getRoundDuration() {
        Time duration = 0;

        // Simulate actual motion of a catom
        for (int i = 0; i < 2 * Rotations3D::nbRotationSteps; i++) {
            duration += Rotations3D::getNextRotationEventDelay();
        }

        return duration;
    }

    ///  Coating
    // static inline constexpr vector<const Cell3DPosition> xset_CCWRelNbh;
    // FIXME: Make non-static
    inline static Cell3DPosition coatingSeed;
    inline static Cell3DPosition spawnLoc;
    // inline static Cell3DPosition closingCorner;
    inline static Cell3DPosition spawnPivot;
    inline static Cell3DPosition spawnBTip;
    inline static Cell3DPosition trainStart;

    inline static const Cell3DPosition zHelperSpawnLoc = Cell3DPosition(5, 5, 2);
    inline static const Cell3DPosition cornerTilePos = Cell3DPosition(3,3,3);

    size_t topCoatingLayer; //!< Height of the top layer coating. FIXME: planar case only.

    bool isOnTheCoatrain = false;
    bool passedThroughCC = false;
    CCWDir lastCCWDir = FrontLeft;
    size_t spawnCount = 0;
    unsigned int currentLayer = 0;
    bool useExternalCoatingOnOddLayers = false;

    vector<Cell3DPosition> closingCorner;
    vector<size_t> resourcesForCoatingLayer;

    // Pre motion coordination
    CoatingBlockCode* lastSpawnedModuleBc = NULL;
    int step = 0; // current rotation
    bool passNextSpawnRound = true;

    // Pending planning
    bool pendingPlanning = false;
    bool pendingPlanningAllowsDirectMotion = true;
    Cell3DPosition pendingPlanningDest;

    // Cheating convergence:
    inline static bool coatingIsOver = false;

    Cell3DPosition determineCoatingSeedPosition() const;
    inline bool isInCSG(const Cell3DPosition& pos) const { return target->isInTarget(pos); };
    bool isInCoating(const Cell3DPosition& pos) const;
    bool isInCoatingLayer(const Cell3DPosition& pos, const unsigned int layer) const;
    bool isInRegularCoatingLayer(const Cell3DPosition& pos, const unsigned int layer) const;
    bool isInOffsetCoatingLayer(const Cell3DPosition& pos, const unsigned int layer) const;
    unsigned int getCoatingLayer(const Cell3DPosition& pos) const;
    bool hasOpenCoatingSlotNeighbor(const unsigned int layer, Cell3DPosition &openSlot) const;
    const vector<CCWDir> getCCWDirectionsFrom(const CCWDir cwd) const;
    size_t getResourcesForCoatingLayer(const unsigned int layer);
    void scheduleNextBorderMotion(bool useInnerBorder = false);
    string CCWDir_to_string(const CCWDir d) const;
    size_t determineTopCoatingHeight() const;

    bool hasNeighborInCSG(const Cell3DPosition& pos) const;
    bool has2ndOrderNeighborInCSG(const Cell3DPosition& pos) const;
    Cell3DPosition nextRotationTowards(const Cell3DPosition& dest,
                                       bool allowDirectMotion = true);

    void handleClosingCornerInsertion();
    void forwardPTNLToSpawnPivot();

    P2PNetworkInterface* getInterfaceToClosingCornerBelow() const;
    bool coatingPositionUnreachable(const Cell3DPosition& pos,
                                    const Cell3DPosition& blkr) const;
    bool verticalLayerShouldOffset(const unsigned int layer) const;
    bool shouldUseExternalCoatingOnOddLayers() const;

    bool isCoatingCorner(const Cell3DPosition& pos) const;
    bool closingCornerInsertionReady(const Cell3DPosition& cc) const;
    Cell3DPosition locateNextClosingCornerFrom(const Cell3DPosition& cc,
                                               bool forceReg = false) const;
    void initializeClosingCornerLocations(vector<Cell3DPosition>& cc,
                                          bool forceReg = false) const;

    /**
     * Indicates whether cells p1 and p2 are opposite, relative to ref
     *
     * @param p1 first cell
     * @param p2 second cell
     * @param ref reference cell
     *
     * @return true if p1 and p2 are opposite relative to ref, false otherwise
     */
    bool cellsAreOpposite(const Cell3DPosition& p1, const Cell3DPosition& p2,
                          const Cell3DPosition& ref) const;

    Cell3DPosition locateFeedingTileRoot() const;
    size_t countBorderCoatingNeighborsInPlace(const Cell3DPosition& pos) const;
    bool coatingSlotInsertionReady(const Cell3DPosition& pos) const;
    CCWDir getCCWDirectionForEdgeBetween(const Cell3DPosition& p1,
                                         const Cell3DPosition& p2) const;

    bool isOnInnerBorderCoating(const Cell3DPosition& pos) const;
};

#endif /* COATING_BLOCKCODE_H_ */
