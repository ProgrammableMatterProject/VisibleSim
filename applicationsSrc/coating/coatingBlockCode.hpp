#ifndef CoatingBlockCode_H_
#define CoatingBlockCode_H_

#include "catoms3DWorld.h"
#include "catoms3DBlockCode.h"

#include "coatingNeighborhood.hpp"

static const int SAMPLE_MSG_ID = 1000;

using namespace Catoms3D;

class CoatingBlockCode : public Catoms3DBlockCode {
private:
    // App-wide Parameters
    static inline bool HIGHLIGHT_COATING = false;
    static inline bool HIGHLIGHT_CSG = false;
    static inline int HIGHLIGHT_COATING_LAYER = -1;
    static inline int HIGHLIGHT_SEEDS = false;
    static inline Cell3DPosition COATING_SEED_POS;
    static inline const Color ATTRACT_DEBUG_COLOR = CYAN;
    static inline const Color AUTH_DEBUG_COLOR = ORANGE;

    // DApp Variables
    Catoms3DBlock *catom;
    BaseSimulator::World *world;
    static inline Neighborhood *neighborhood;
    static inline std::function<bool (const Cell3DPosition&)> isInG;
    static inline multimap<Cell3DPosition, function<void (const Cell3DPosition&)>> watchlist;
public :
    CoatingBlockCode(Catoms3DBlock *host);
    ~CoatingBlockCode() {
        delete neighborhood;
    };

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
    static int getCoatingLayer(const Cell3DPosition& pos);

    /**
     * @param pos position to evaluate
     * @return true if pos is inside the current CSG description, false otherwise
     */
    static bool isInCSG(const Cell3DPosition& pos) {
        return BaseSimulator::getWorld()->lattice->isInGrid(pos) and target->isInTarget(pos);
    }

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
     * @param dir
     * @return true if module has a neighbor in lattice direction dir
     */
    bool hasNeighborInDirection(SkewFCCLattice::Direction dir) const;

    /**
     * Requests to be notified when position pos is ready to be filled
     * @param requester module to which the request will be sent
     * @param d the direction of the target position relative to requester
     * @return true if the position is already ready to be filled, false otherwise
     */
    bool getAuthorizationToAttract(const Cell3DPosition& requester, CCWDir d);

    /**
     * Same as CoatingBlockCode::getAuthorizationToAttractTo, but using border following
     *  to reach the destination authorizer
     * @param pos
     * @return true if the position is already ready to be filled, false otherwise
     */
    bool borderFollowingAttractRequestTo(const Cell3DPosition& pos);

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
