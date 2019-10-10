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

#define IT_MODULE_INSERTION 1

enum CWDir {FrontLeft, Front, FrontRight, Right, RearRight, Rear, RearLeft, Left };

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
    inline static bool sandboxInitialized;

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

    bool parseUserCommandLineArgument(int argc, char *argv[]) override;

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
    inline static const int NumCWDirs = 8;
    inline static constexpr Cell3DPosition CWDPos[NumCWDirs] = {
        Cell3DPosition(-1, -1, 0), // FrontLeft
        Cell3DPosition(0, -1, 0), // Front
        Cell3DPosition(1, -1, 0), // FrontRight
        Cell3DPosition(1, 0, 0), // Right
        Cell3DPosition(1, 1, 0), // RearRight
        Cell3DPosition(0, 1, 0), // Rear
        Cell3DPosition(-1, 1, 0), // RearLeft
        Cell3DPosition(-1, 0, 0), // Left
    };

    inline static constexpr Cell3DPosition diagNeighbors[4] = { Cell3DPosition(-1,-1,0),
        Cell3DPosition(1,-1,0), Cell3DPosition(-1,1,0), Cell3DPosition(1,1,0), };

    // static inline constexpr vector<const Cell3DPosition> xset_CWRelNbh;
    inline static Cell3DPosition spawnLoc;
    inline static Cell3DPosition closingCorner;
    inline static Cell3DPosition spawnPivot;

    CWDir lastCWDir = FrontLeft;
    int spawnCount = 0;
    int currentLayer = 0;

    inline bool isInCSG(const Cell3DPosition& pos) const { return target->isInTarget(pos); };
    bool isInCoatingLayer(const Cell3DPosition& pos, const int layer) const;
    int getCoatingLayer(const Cell3DPosition& pos) const;
    bool hasOpenCoatingSlotNeighbor(const int layer, Cell3DPosition &openSlot) const;
    const vector<CWDir> getCWDirectionsFrom(const CWDir cwd) const;
    int getResourcesForCoatingLayer(const int currentLayer);
    string CWDir_to_string(const CWDir d) const;
};

#endif /* COATING_BLOCKCODE_H_ */
