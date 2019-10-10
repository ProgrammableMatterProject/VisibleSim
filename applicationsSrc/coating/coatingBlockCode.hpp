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

#define IT_MODE_TILEROOT_ACTIVATION 1
#define IT_MODE_ALGORITHM_START 2

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

    inline static int nbSandboxCatoms = 0;
    inline static int nbOpenScaffoldPositions = 0;
    inline static int nbCSGCatomsInPlace = 0;
    inline static int nbModulesInShape = 0;

    inline static int nbSeedTiles = 0;
    inline static int nbPathsOver = 0;
    inline static Time reconfTime = 0;

    inline static int nbMessages = 0;
    inline static Time t0 = 0;
    inline static bool NO_FLOODING = true;
    inline static bool BUILDING_MODE = false; // const after call to parseUserCommandLineArgument
    inline static bool HIGHLIGHT_CSG = false;
    inline static bool HIGHLIGHT_SCAFFOLD = false;
    inline static bool sandboxInitialized;

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

    inline static Time getRoundDuration() {
        Time duration = 0;

        // Simulate actual motion of a catom
        for (int i = 0; i < 2 * Rotations3D::nbRotationSteps; i++) {
            duration += Rotations3D::getNextRotationEventDelay();
        }

        return duration;
    }

    /**
     * @return the position of the tile root module of the tile to which this module belongs
     */
    Cell3DPosition getTileRootPosition(const Cell3DPosition& pos) const;

    Scheduler *scheduler;
    World *world;
    Lattice *lattice;
    Catoms3D::Catoms3DBlock *catom;
    CoatingRuleMatcher *ruleMatcher;

    BranchIndex branch;
    AgentRole role;
    Cell3DPosition coordinatorPos;
    Cell3DPosition targetPosition;
    bool rotating = false;

#define SET_GREEN_LIGHT(x) setGreenLight(x, __LINE__)

    /**
     * Changes the light state of a pivot and take the appriopriate action
     */
    void setGreenLight(bool onoff, int _line_);
    bool isAdjacentToPosition(const Cell3DPosition& pos) const;

    /**
     * Transforms a shifted grid position into a mesh absolute position.
     * @note This has to be used due to the mesh seed being offsetted in order to leave space
     *  for spawning modules
     * @param pos position to normalize
     * @return the corresponding position of pos in the coordinate system of the mesh
     */
    static const Cell3DPosition norm(const Cell3DPosition& pos);
    static const Cell3DPosition denorm(const Cell3DPosition& pos);


    /**
     * Add initial sandbox modules to the lattice
     */
    void initializeSandbox();

    bool isInsideCSGFn(const Cell3DPosition& pos) const;

    void scheduleRotationTo(const Cell3DPosition& pos, Catoms3DBlock* pivot);

    void highlightCSGScaffold(bool force = false);
};

#endif /* COATING_BLOCKCODE_H_ */
