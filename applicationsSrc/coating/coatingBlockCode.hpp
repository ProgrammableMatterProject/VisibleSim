#ifndef CoatingBlockCode_H_
#define CoatingBlockCode_H_

#include "catoms3DWorld.h"
#include "catoms3DBlockCode.h"

#include "coatingNeighborhood.hpp"
#include "coatingBorder.hpp"
#include "coatingSeeding.hpp"
#include "scaffoldManager.hpp"

#include <set>

static const int SAMPLE_MSG_ID = 1000;

using namespace Catoms3D;

class CoatingException : public VisibleSimException {
public:
    CoatingException(const string& msg) : VisibleSimException(msg, std::string("Coating")) {}
};

class CoatingBlockCode : public Catoms3DBlockCode {
// private:
public :
    // App-wide Parameters
    static inline unsigned int B = 6;
    static inline unsigned int GroundZ = 3;

    static inline bool HIGHLIGHT_COATING = false;
    static inline bool HIGHLIGHT_CSG = false;
    static inline int HIGHLIGHT_COATING_LAYER = -1;
    static inline int HIGHLIGHT_SEEDS = false;
    static inline int HIGHLIGHT_SUPPORTS = false;
    static inline int HIGHLIGHT_SCAFFOLD = false;

    static inline bool COATING_MODE = false;

    static inline Cell3DPosition G_SEED_POS;

    static inline const Color ATTRACT_DEBUG_COLOR = CYAN;
    static inline const Color AUTH_DEBUG_COLOR = ORANGE;

    static inline Time ATTRACT_DELAY = 10;

    static inline const Color WaitingColor = BLUE;
    static inline const Color AttractedColor = GREEN;
    static inline const Color SupportColor = ORANGE;
    static inline const Color DefaultColor = DARKGREEN;
    static inline const Color InvalidColor = Color(156, 0, 0, 255, true);

    // DApp Variables
    Catoms3DBlock *catom;
    BaseSimulator::World *world;

    static inline bool sandboxInitialized = false;

    static inline Neighborhood *neighborhood;
    static inline Border *border;
    static inline Seeding *seeding;
    static inline ScaffoldManager *scaffold;

    static inline Cell3DPosition scaffoldSeed = Cell3DPosition(3,3,3);

    static inline std::function<bool (const Cell3DPosition&)> isInG;
    static inline multimap<Cell3DPosition, function<void (void)>> watchlist;
    static inline set<Cell3DPosition> waitingModules;

    static inline int nPlanes; //!< Number of planes
    static inline vector<set<Cell3DPosition>> planeSupports; //!< Support positions for plane
    static inline vector<unsigned int> planeSupportsReady; //!< Support in ready state for plane

    static inline vector<int> planeRequires; //!< Number of modules plane i needs
    static inline vector<int> planeAttracted; //!< Number of modules plane i has attracted
    static inline vector<set<Cell3DPosition>> planeSeed; //!< Modules responsible for attracting the first module of the plane above them, one per contiguous plane, potentially multiple per layer therefore

    // FIXME: non-local, should be a gradient value
    static inline set<Cell3DPosition> attractedBySupport; //!< Set of all modules that have been attracted by a support rather than through the regular road. It fakes a gradient signal that would contain whether or not this is a regular gradient or a support gradient

    // FIXME: non-local, should be a gradient value
    static inline set<Cell3DPosition> borderCompleted; //!< Set of all modules that have been attracted through border completion

    set<Cell3DPosition> expectedSegments; //!< Roots of segments grown from a support module
    unsigned short numCompletedSegments = 0; //!< Number of completion ACK rcvd from segments
    set<Cell3DPosition> segmentsAckBlacklist; //!< Used to maintain convergence/border following

    bool handledBorderCompletion = false; //!< True if module has already attracted its neighbor during the border completion algorithm
    // bool expectingCompletionNeighbor = false; //!< indicates that the modules has a attracted a module to a neighbor spot for border completion and is waiting to send it a message
    set<Cell3DPosition> completionNeighbors; //!< the positions that the module has attracted and is monitoring
    P2PNetworkInterface *prevBorderItf = nullptr; //!< Previous itf module is waiting on for border completion
    P2PNetworkInterface *nextBorderItf = nullptr; //!< Next itf module is waiting on for border completion
    bool builtScaffold = false; //!< Indicates if module has attempted to attract scaffold neighbors

    Cell3DPosition lastBorderFollowingPosition;
    P2PNetworkInterface *supportReadyRequestItf = nullptr; //!< Awaiting request if != nullptr
    bool segmentsDetected = false;
    set<Cell3DPosition> supportsReadyBlacklist; //!< Used to avoid looping
public :
    CoatingBlockCode(Catoms3DBlock *host);
    ~CoatingBlockCode();

    /**
     * This function is called on startup of the blockCode, it can be used to perform initial
     *  configuration of the host or this instance of the program.
     * @note this can be thought of as the main function of the module
     **/
    void startup() override;

    /**
     * @brief Handler for all events received by the host block
     * @param pev pointer to the received event
     */
    void processLocalEvent(EventPtr pev) override;

    /**
     * @brief Callback function executed whenever the module finishes a motion
     */
    void onMotionEnd() override;

    /**
     * @brief Sample message handler for this instance of the blockcode
     * @param msgPtr Pointer to the message received by the module
     * @param sender Connector of the module that has received the message and that is connected to the sender */
    void handleSampleMessage(MessagePtr msgPtr, P2PNetworkInterface *sender);

    /**
     * Highlights the CSG elements or coating as requested by the user
     */
    void highlight() const;

    /**
     * Indicates whether position pos is in the coating or not
     * @param pos position to evaluate
     * @return true if pos is in the coating of the current shape, false otherwise
     */
    static bool isInCoating(const Cell3DPosition& pos);

    /**
     * Indicates whether position pos is in the coating at a given layer or not
     * @param pos position to evaluate
     * @param layer layer to evaluate, starts from 0 (i.e., coating seed layer)
     * @return true if pos is in the coating of the shape at the given layer, false otherwise
     */
    static bool isInCoatingLayer(const Cell3DPosition& pos, int layer);

    /**
     * @param pos position to evaluate
     * @return the coating layer to which pos belongs
     */
    static int getGLayer(const Cell3DPosition& pos);

    /**
     * @param pos position to evaluate
     * @return true if pos is inside the current CSG description, false otherwise
     */
    static bool isInCSG(const Cell3DPosition& pos) {
        return BaseSimulator::getWorld()->lattice->isInGrid(pos) and target->isInTarget(pos);
    }

    static bool isOnCSGBorder(const Cell3DPosition& pos);

    /**
     * @param pos position to evaluate
     * @return true if pos has a neighbor position that is inside the CSG description
     */
    static bool hasHorizontalNeighborInCSG(const Cell3DPosition& pos);

    /**
     * @param pos position to evaluate
     * @return true if pos has a second order neighbor position inside the CSG description
     */
    static bool has2ndOrderNeighborInCSG(const Cell3DPosition& pos);

    /**
     * Attract modules as enabled by the self-assembly rules
     */
    void attract();

    /**
     * Sends an attract signal to add a module to position pos of the target shape
     * @param pos target position
     */
    void sendAttractSignalTo(const Cell3DPosition& pos);

    /**
     * Creates a list of all structural supports
     */
    void initializeStructuralSupports();

    /**
     * Attracts all structural supports for the given layer to their positions.
     * @param layer
     */
    void attractStructuralSupports(int layer);

    /**
     * Requests to be notified when position pos is ready to be filled
     * @param requestee module to which the request will be sent
     * @param d the direction of the check position relative to requester
     * @return true if the position is already ready to be filled, false otherwise
     */
    bool getAuthorizationToAttract(const Cell3DPosition& requestee, PlanarDir d);

    /**
     * Same as CoatingBlockCode::getAuthorizationToAttractTo, but using border following
     *  to reach the destination authorizer
     * @param pos
     * @param d the direction of the desired insertion by this module
     * @return true if the position is already ready to be filled according to the requestee
     *  , false otherwise
     */
    bool borderFollowingAttractRequest(const Cell3DPosition& requestee,
                                       PlanarDir d);

    /**
     * Initializes the number of modules expected for each plane as well as
     *  their respective seeds
     */
    void initializePlaneSeeds();

    /**
     * If sandbox tiles are missing, initialize them by adding them to the reconfiguration
     *  scene across the entire ground
     */
    void initializeSandbox();

    bool borderHasWaitingModule(int startIdx) const;

    /**
     * @param pos
     * @return true if position pos has two horizontal neighbor position that are both
     *  orthogoal and in G
     */
    static bool hasOrthogonalNeighborsInCSG(const Cell3DPosition& pos);

    /**
     * @param pos position to evaluate
     * @return true if pos is a support position that glues the scaffold to the coating
     */
    static bool isSupportPosition(const Cell3DPosition& pos);

    /**
     * @param a
     * @param b
     * @return true if cell a is part of the coating and would be blocked by cell b as
     *  it may have a neighbor opposite to its connector attached to b
     */
    bool coatingCellWillBeBlockedBy(const Cell3DPosition& a, const Cell3DPosition& b) const;

    /**
     * @param pos
     * @return true if position pos cannot be blocked by two opposing coating modules
     */
    bool isCoatingCornerCell(const Cell3DPosition& pos) const;

    /**
     * Called by a module that has just been attracted by a support module, or one which
     *  belongs to one of the modules of the segment grown by this support so as to avoid
     *  leaving blocked positions on the border.
     * This function evaluates whether the current segment should be extended by one module
     *  (depending wether the next module along the segment is a corner or not), and attracts
     *  it if needed.
     */
    void attractNextModuleAlongSegment();

    /**
     * @param pos position to evaluate
     * @return true if pos is on a segment that is to be built by a support module
     */
    bool isOnSupportSegment(const Cell3DPosition& pos) const;

    /**
     * Updates cur to the end of coating cell segment starting at from and in direction dir
     * @param dir direction of navigation
     * @param cur position to update
     * @param from position to start from
     */
    void reachEndOfSegmentInDirection(short dir, Cell3DPosition& cur,
                                      const Cell3DPosition& from) const;

    /**
     * Initiates the algorithm that fills the missing holes between support segments
     */
    void startBorderCompletionAlgorithmFromSeed(const Cell3DPosition& seed);

    /**
     * Attracts the first modules of the next plane, or if some of those are on a support
     *  segment, calls startBorderCompletionAlgorithm() to initiate segment completion
     * @param layer plane layer to attract
     */
    void attractPlane(unsigned int layer);

    /**
     * @param previous module on the border
     * @param additional predicate that the target position must verify
     * @return the next coating position along the border in the direction
     *  previous->catom->next for the current layer
     */
    Cell3DPosition
    findNextCoatingPositionOnLayer(const Cell3DPosition& previous,
                                   const std::function<bool(const Cell3DPosition&)> pred =
                                   [](const Cell3DPosition& p) { return true; }) const;

    /**
     * @param set of modules to disregard
     * @param additional predicate that the target position must verify
     * @return the next coating position along the border in the direction
     *  previous->catom->next for the current layer
     */
    Cell3DPosition
    findNextCoatingPositionOnLayer(const set<Cell3DPosition>& previous,
                                   const std::function<bool(const Cell3DPosition&)> pred =
                                   [](const Cell3DPosition& p) { return true; }) const;

    /**
     * Attracts scaffold neighbors if scaffold had not been initialized
     */
    void assembleInternalScaffoldNeighbors();

    /**
     * Searches for the first coating position on the diagonal and then for the first corner
     *  position rotating CCW from there. Set that point as G_SEED_POS and start coating from
     *  there by inserting that module
     */
    void initializeGSeedPosition();

    /**
     * Start the border completion algorithm in the normal direction, as well as until
     *  the next corner module in the other direction
     */
    void startBorderCompletion();

    /**
     * Finds the next position on the prefilled border and either add the next module along it
     * @param sender of the border completion message, or
     * @param stopAtCorner if stopAtCorner is active, complete backward until next corner
     *                      else only go forward
     */
    void handleBorderCompletion(const Cell3DPosition& pos, bool stopAtCorner);

    /**
     * Sends a message to
     */
    void notifyAttracterOfSegmentCompletion(set<Cell3DPosition>& blacklist,
                                            P2PNetworkInterface *sender = nullptr);

    /**
     * @param pos
     * @return true if pos is a seed position
     */
    bool isSeedPosition(const Cell3DPosition& pos) const;

    /**
     * @param seed A seed module
     * @return the first position of the plane above the seed module
     */
    Cell3DPosition getStartPositionAboveSeed(const Cell3DPosition& seed) const;

    /**
     * Sends a message to the first module of the next plane that is already in place
     *  (is a segment module), and engage border completion
     */
    void startBorderCompletionAlgorithm();

    inline static bool isInScaffold(const Cell3DPosition& pos) {
        return scaffold->isInScaffold(scaffold->normalize(pos)) and scaffold->isInsideFn(pos);
    }

    /// Advanced blockcode handlers below

    /**
     * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from it.
     * @param config : pointer to the TiXmlDocument representing the configuration file, all information related to VisibleSim's core have already been parsed
     *
     * Called from BuildingBlock constructor, only once.
     */
    void parseUserElements(TiXmlDocument *config) override {}

    /**
     * User-implemented debug function that gets called when a module is selected in the GUI
     */
    void onBlockSelected() override;

    /**
     * User-implemented debug function that gets called when a VS_ASSERT is triggered
     * @note call is made from utils::assert_handler()
     */
    void onAssertTriggered() override;

    /**
     * User-implemented keyboard handler function that gets called when
     *  a key press event could not be caught by openglViewer
     * @param c key that was pressed (see openglViewer.cpp)
     * @param x location of the pointer on the x axis
     * @param y location of the pointer on the y axis
     * @note call is made from GlutContext::keyboardFunc (openglViewer.h)
     */
    void onUserKeyPressed(unsigned char c, int x, int y) override {}

    /**
     * Call by world during GL drawing phase, can be used by a user
     *  to draw custom Gl content into the simulated world
     * @note call is made from World::GlDraw
     */
    void onGlDraw() override {}

    /**
     * @brief This function is called when a module is tapped by the user. Prints a message to the console by default.
     Can be overloaded in the user blockCode
     * @param face face that has been tapped */
    void onTap(int face) override {}

    bool parseUserCommandLineArgument(int& argc, char **argv[]) override;

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new CoatingBlockCode((Catoms3DBlock*)host));
    };
/*****************************************************************************/
};

#endif /* CoatingBlockCode_H_ */
