/**
 * @file   scaffoldingBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jun 11 15:28:09 2019
 *
 * @brief
 *
 *
 */


#ifndef MESHCATOMS3DBLOCKCODE_H_
#define MESHCATOMS3DBLOCKCODE_H_

#include <deque>
#include <unordered_set>
#include <set>

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DMotionRules.h"
#include "rotation3DEvents.h"
#include "catoms3DBlock.h"
#include "cell3DPosition.h"

#include "scaffoldingMessages.hpp"
#include "scaffoldingRuleMatcher.hpp"

#include "scaffoldingLocalRules.hpp"

#define IT_MODE_TILEROOT_ACTIVATION 1
#define IT_MODE_ALGORITHM_START 2
#define IT_MODE_FINDING_PIVOT 3 // FIXME: TODO:

class ScaffoldingBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
    static const int B = B2; // defined in MeshLocalRules for convenience
    inline static int X_MAX = numeric_limits<int>::min();
    inline static int Y_MAX = numeric_limits<int>::min();
    inline static int Z_MAX = numeric_limits<int>::min();
    inline static int X_MIN = numeric_limits<int>::max();
    inline static int Y_MIN = numeric_limits<int>::max();
    inline static int Z_MIN = numeric_limits<int>::max();
    inline static constexpr Cell3DPosition meshSeedPosition = Cell3DPosition(3,3,3);
    static int nbCatomsInPlace;
    static int nbMessages;
    static Time t0;

    inline static const bool NO_FLOODING = true;

    // For stats export
    pair<int, string> maxBitrate;
    pair<Time, int> rate = { 0, 0 };

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

    inline const Cell3DPosition getTileRelativePosition() const {
        return catom->position - coordinatorPos;
    }

    inline BranchIndex getBranchIndex(const Cell3DPosition& pos) const {
        return ruleMatcher->getBranchIndexForNonRootPosition(
            norm(pos) + (norm(pos)[2] < 0 ? Cell3DPosition(0,0,B) : Cell3DPosition(0,0,0)));
    }

    inline bool isInSandbox(const Cell3DPosition& pos) const {
        return pos[2] < meshSeedPosition[2];
    }

    int debugColorIndex = 0;

    Scheduler *scheduler;
    World *world;
    Lattice *lattice;
    Catoms3D::Catoms3DBlock *catom;
    MeshCoating::ScaffoldingRuleMatcher *ruleMatcher;

    /** MESSAGE ROUTING **/

    /** CONTINUOUS FEEDING **/
    bool moduleWaitingOnBranch[4] = { false, false, false, false};

    /**
     * This is used only by branch tips. This is used to prevent double sending
     *  REQUEST_TARGET_CELL for a single module, as can happen if the coordinator
     *  gets into place while the RTC message is being sent, forcing the requesting module
     *  to respond to the COORDINATOR_READY message even though it has already received its
     *  request. This variable prevents the tip from forwarding back the CR
     *  if the tip has already sent the request to the coordinator.
     */
    bool sentRequestToCoordinator = false;

    /**
     * Actually the previous is not sufficient to prevent double sends.
     * This does not seem possible without acknowledgements. Which are too heavy.
     * Therefore simply prevent coordinator from responding to double requests using:
     */
    std::unordered_set<bID> processedRQId;

    /**
     * ONLY FOR GROUND TR. This is to ensure that spawned module only
     * arrive once light is green on pivot module, as would happen on a real sandbox
     * @attention indexing is done based on the id of the incident branch to which the
     *  EPL belongs. Therefore the index of the EPLPivot under ZBranch is 1 (RevZBranch)
     */
    ScaffoldingBlockCode* EPLPivotBC[4] = { NULL, NULL, NULL, NULL };

    /**
     * Indicates how many incident branches the current tile has.
     * Four incident branches feeding it produces the best
     *  construction speed and convenience is optimal
     */
    short numIncidentVerticalBranches = 0;

    /**
     * matchingLocalRule is set to true if a module has received a PROVIDE_TARGET_CELL
     *  but could match any local rule upon reception. Perhaps the correct environmental
     *  conditions are not met yet. This variable forces local rules reevaluation upon
     *  any local neighborhood update.
     */
    bool matchingLocalRule = false;

    /**
     * Identifier of the last visited EPL, used for local rules matching until
     *  module reaches another EPL or a component
     */
    short lastVisitedEPL = -1;

    /**
     * Indicates whether the module using this pivot as support is on its final rotation
     *  (if true), or whether it will take additional rotations afterwards.
     * If true, the pivot can turn green directly once it is done actuating, even though
     * the moving module will still be docked to it. //FIXME: Not useful if finaltargetreachedmessage is being used
     */
    bool finalPositionForActuatedModule = false;

    /**
     * Position of the pivot module that is helping this module move.
     * It can be useful to keep track off as the moved module can wish to send a message
     *  to its pivot after motion, as is the case for telling a pivot to turn green because
     *  even though it is still docked, its final position as been reached.
     */
    Cell3DPosition pivotPosition;

    /**
     * Custom version of findMotionPivot for the scaffolding that only considers
     *  final modules part of the scaffold as pivot
     */
    static Catoms3DBlock*
    customFindMotionPivot(const Catoms3DBlock* m,
                          const Cell3DPosition& tPos,
                          RotationLinkType faceReq = RotationLinkType::Any);

    bool initialized = false; //!< Indicates whether this module has called startup() yet

    /**
     * Queue of position to be filled my incoming modules, as an ordered list of
     *  pair<TileElement, SourceEPL>, which means that TileElement will be built
     *  from a module coming through SourceEPL.
     * The queue is populated dynamically by ScaffoldingBlockCode::buildConstructionQueue
     */
    std::deque<std::pair<ScafComponent, ScafComponent>> constructionQueue;

    /**
     * Dynamically builds the construction queue based on the position of the tile.
     */
    deque<pair<ScafComponent, ScafComponent>>
    buildConstructionQueue(const Cell3DPosition& pos) const;
    deque<pair<ScafComponent, ScafComponent>>
    buildConstructionQueueWithFourIncidentBranches(const Cell3DPosition& pos) const;
    deque<pair<ScafComponent, ScafComponent>>
    buildConstructionQueueWithFewerIncidentBranches(const Cell3DPosition& pos) const;

    /**
     * Indicates whether an EPL pivot module has received a TileInsertionReady message
     *  before a module had arrived on its EPL neighbor cell. In that case, delay
     *  forwarding the TileInsertionReady message until a module arrives on its EPL site.
     */
    bool tileInsertionPending = false;

    /**
     * Used to wait when a pivot cannot be found atm, which likely exposes an issue
     *  with the coordination mechanism.
     * FIXME:TODO:
     */
    bool notFindingPivot = false;
    int notFindingPivotCount = 0;

    /**
     * For a module that just got into final position, keeps track of the number
     *  of ADD_NEIGHBOR_EVENT to process before the module can behave normally.
     * This is to avoid state inconsistencies at the moment of arrival (errors due to
     *  adding a neighbor on a RED pivot for example)
     */
    int addNeighborToProcess = 0;

    /**
     * Use to limit interruption events after top level S_RevZ arrived
     *  so that the simulation ends nicely for stat export
     */
    static bool constructionOver;

    /**
     * Used to ensure that only one module on the RevZBranch train can claim the R position.
     *  This is stored in RevZ pivots from Z_EPL to the R position. It is set to true
     *  when a pivot responds to a ProbePivotLight for the R position, and all further
     *  requests for a motion to R will be ignored
     */
    bool RModuleRequestedMotion = false;

#define SET_GREEN_LIGHT(x) setGreenLight(x, __LINE__)
    /**
     * Changes the light state of a pivot and take the appriopriate action
     */
    void setGreenLight(bool onoff, int _line_);

    Time randomRotationTimeStart = 0;

    inline static set<Cell3DPosition> claimedTileRoots;

    /** END CF **/

    /** MOTION COORDINATION **/
    bool greenLightIsOn = true;
    bool moduleAwaitingGo = false;
    Cell3DPosition awaitingModulePos = Cell3DPosition(-1, -1, -1);
    P2PNetworkInterface *awaitingModuleProbeItf = NULL;
    Cell3DPosition actuationTargetPos;
    Cell3DPosition stepTargetPos;
    Catoms3DBlock *stepPivot;

    bool rotating = false;

    bool isAdjacentToPosition(const Cell3DPosition& pos) const;
    // NOTE: what if there is more than 1?
    Catoms3DBlock* findTargetLightAmongNeighbors(const Cell3DPosition& targetPos,
                                                 const Cell3DPosition& srcPos) const;
    Catoms3DBlock* findTargetLightAroundTarget(const Cell3DPosition& targetPos,
                                               const Cell3DPosition& finalPos) const;

    void setGreenLightAndResumeFlow();
    /**                     **/


    BranchIndex branch;
    AgentRole role;
    Cell3DPosition coordinatorPos;
    Cell3DPosition targetPosition;

    Time startTime;

    // Free Agent Vars
    short step = 1; // For moving FreeAgents
    bool tileInsertionAckGiven = false; // for tile insertion coordination at HBranch tips

    // Support Agent Vars
    Cell3DPosition branchTipPos; // For Support role only

    // Coordinator Vars
    std::array<int, 4> catomsSpawnedToVBranch = { 0, 0, 0, 0 }; // Number of catoms fed to each vertical branch
    std::array<int, 6> catomsReqByBranch = {-1,-1,-1,-1,-1,-1}; // We could have -1 if branch should not be grown
    std::array<bool, 6> moduleReadyOnEPL = {0}; //<! keeps track of modules which arrived on Tile Entry Point Locations


    /**
     * Finds the next target position that a module arriving at EPL epl should move to,
     *  and return it
     * @param epl entry point location to evaluate
     * @return the next target cell that should be filled by catom at epl
     */
    // const Cell3DPosition getNextTargetForEPL(ScafComponent epl);

    // TargetCSG *target;
    ScaffoldingBlockCode(Catoms3D::Catoms3DBlock *host);
    ~ScaffoldingBlockCode();

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

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new ScaffoldingBlockCode((Catoms3DBlock*)host));
    }

    /**
     * Add initial sandbox modules to the lattice
     */
    void initializeSandbox();
    static bool sandboxInitialized;

    /**
     * Transforms a shifted grid position into a mesh absolute position.
     * @note This has to be used due to the mesh seed being offsetted in order to leave space
     *  for spawning modules
     * @param pos position to normalize
     * @return the corresponding position of pos in the coordinate system of the mesh
     */
    static const Cell3DPosition norm(const Cell3DPosition& pos);

    /**
     * norm variant to be used with sandbox modules as their are not considered inside the
     *  mesh by the ScaffoldingRuleMatcher module
     */
    static const Cell3DPosition sbnorm(const Cell3DPosition& pos);

    /**
     * Inverse function of norm
     * @note This has to be used due to the mesh seed being offsetted in order to leave space
     *  for spawning modules
     * @param pos position to denormalize
     * @return the corresponding position of pos in the absolute coordinate system
     */
    static const Cell3DPosition denorm(const Cell3DPosition& pos);

    /**
     * denorm variant to be used with sandbox modules as their are not considered inside the
     *  mesh by the ScaffoldingRuleMatcher module
     */
    static const Cell3DPosition sbdenorm(const Cell3DPosition& pos);

    bool isInsideCSGFn(const Cell3DPosition& pos) const;

    /**
     * Adds a new mesh component to the grid at its predetermined entry point,
     *  and prepare to send it its coordinates
     * @param mc mesh component of catom to add
     */
    void handleScafComponentInsertion(ScafComponent mc);

    bool handleModuleInsertionToIncidentBranch(BranchIndex bid);

    /**
     * Checks if all the incident branches around a tile root are complete
     * @param pos position of the tile root
     * @return true if all incident branches are complete, false otherwise
     */
    bool incidentBranchesToRootAreComplete(const Cell3DPosition& pos);

    /**
     * Checks if branch tip from branch bi is in place around tile root at trp
     * @param trp tile root position
     * @param bi index of the incident branch that needs checking
     * @return true if the tip of branch bi is in place next ot trp
     */
    bool isIncidentBranchTipInPlace(const Cell3DPosition& trp, BranchIndex bi);

    void scheduleRotationTo(const Cell3DPosition& pos, Catoms3DBlock* pivot);

    std::bitset<12> getMeshLocalNeighborhoodState();
    void matchRulesAndRotate();
    void matchRulesAndProbeGreenLight();

    void initializeTileRoot();
    void initializeSupportModule();

    /**
     * Locates a route for communicating with the tile root and sends
     *  a RequestTargetCell message through that route
     * @return true if a message could be sent, false otherwise
     */
    bool requestTargetCellFromTileRoot();

    /**
     * Removes the next element in the target cell queue of the EPL for component comp
     * @param comp component for which the EPL queue must be popped
     */
    void discardNextTargetForComponent(ScafComponent comp);

    /**
     * Check each EPL on the incident branches to decide whether or not to introduce
     *  a new module on that branch's EPL. EPLPivot must light be green, and EPL empty.
     */
    void feedIncidentBranches();

    /**
     * @return true if catom is on the lowest tile layer, false otherwise
     */
    bool isAtGroundLevel();
    /**
     * @copydoc BlockCode::sendMessage
     * @note This is only used for logging sent messages, it calls
     *  BlockCode::sendMessage immediatly after
     */
    virtual int sendMessage(HandleableMessage *msg,P2PNetworkInterface *dest,
                            Time t0,Time dt) override;

    /**
     * @copydoc BlockCode::sendMessage
     * @note This is only used for logging sent messages, it calls
     *  BlockCode::sendMessage immediatly after
     */
    virtual int sendMessage(Message *msg,P2PNetworkInterface *dest,
                            Time t0,Time dt);

    void log_send_message() const;
    void updateMsgRate();

    /**
     * @return true if module at entry point location epl is immediately required for the construction, or false if it should wait
     */
    bool moduleAtEPLIsRequiredAtOnce(ScafComponent epl);

    /**
     * Checks for every module awaiting at an EPL, if that module is required for the next construction steps,and if so sends it a PROVIDE_TARGET_CELL message to resume the flow.
     */
    void awakenPausedModules();
    std::array<bool, 12> moduleAwaitingOnEPL = {0};

    /**
     * @param dst target position to get closer to
     * @param upward whether to consider upward branches (of the tile)
     *  or downward branches (parent)
     * @return The branch index of the branch that leads the closest to dst
     */
    BranchIndex findBestBranchIndexForMsgDst(const Cell3DPosition& dst,
                                             bool upward) const;

    /*********************************************************************/
    /*********************** NO FLOODING STUFF ***************************/
    /*********************************************************************/

    std::map<ScafComponent, int> sandboxResourcesRequirement; //!< returns the number of modules to be spawned by the current coordinator at the sandbox level on epl ScafComponent

    int resourcesForTileThrough(const Cell3DPosition& pos, ScafComponent epl) const;
};

#endif /* MESHCATOMS3DBLOCKCODE_H_ */
