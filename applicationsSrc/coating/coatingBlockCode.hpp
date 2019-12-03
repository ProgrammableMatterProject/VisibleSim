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
    static inline Cell3DPosition COATING_SEED_POS = Cell3DPosition(2, 2, 2);

    // DApp Variables
    Catoms3DBlock *catom;
    Neighborhood *neighborhood;
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
    bool isInCoating(const Cell3DPosition& pos) const;

    /**
     * Indicates whether position pos is in the coating at a given layer or not
     * @param pos position to evaluate
     * @param layer layer to evaluate, starts from 0 (i.e., coating seed layer)
     * @return true if pos is in the coating of the shape at the given layer, false otherwise
     */
    bool isInCoatingLayer(const Cell3DPosition& pos, int layer) const;

    /**
     * @param pos position to evaluate
     * @return the coating layer to which pos belongs
     */
    int getCoatingLayer(const Cell3DPosition& pos) const;

    /**
     * @param pos position to evaluate
     * @return true if pos is inside the current CSG description, false otherwise
     */
    static bool isInCSG(const Cell3DPosition& pos) { return target->isInTarget(pos); }

    /**
     * @param pos position to evaluate
     * @return true if pos has a neighbor position that is inside the CSG description
     */
    bool hasHorizontalNeighborInCSG(const Cell3DPosition& pos) const;

    /**
     * @param pos position to evaluate
     * @return true if pos has a second order neighbor position inside the CSG description
     */
    bool has2ndOrderNeighborInCSG(const Cell3DPosition& pos) const;

    /**
     * Attract modules as enabled by the self-assembly rules
     */
    void attract();

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
