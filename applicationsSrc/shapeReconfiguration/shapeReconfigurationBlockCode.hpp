#ifndef ShapeReconfigurationBlockCode_H_
#define ShapeReconfigurationBlockCode_H_

#include "robots/hexanodes/hexanodesMotionEvents.h"
#include "robots/hexanodes/hexanodesMotionEngine.h"
#include "robots/hexanodes/hexanodesSimulator.h"
#include "robots/hexanodes/hexanodesWorld.h"
#include "robots/hexanodes/hexanodesBlockCode.h"
#include "grid/lattice.h"

static const int DISTANCE_BROADCAST_MSG_ID = 1000;
static const int DISTANCE_FORECAST_MSG_ID = 1001;
static const int LEADER_BROADCAST_MSG_ID = 1002;

using namespace Hexanodes;

class ShapeReconfigurationBlockCode : public HexanodesBlockCode {
private:
    HexanodesBlock *module;
    int myDistance = 0;
    int myNextLeafId;
    int myCurrentRound = 0;
    int myNbWaitedAnswers = 0;
    bool isLeader = false;  
    int myMaxDistanceToLeaf = 0;
    bool isInMotion = false;
    bool isInPosition = false;
    P2PNetworkInterface *myParent = nullptr;
    vector<P2PNetworkInterface*> myChildren = {};
    Cell3DPosition initialPosition;
public :
    ShapeReconfigurationBlockCode(HexanodesBlock *host);
    ~ShapeReconfigurationBlockCode() {};
    inline static size_t nbMotions = 0;
    inline static string msg = "I have reached an obstacle!!!!";
    inline static bool robotAtObstacle = false;
    enum motionDirectionStatic{CCW,CW};
    inline static motionDirectionStatic CurrentDirection = motionDirectionStatic::CW;

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
     * @param _msg Pointer to the message received by the module, requires casting
     * @param sender Connector of the module that has received the message and that is connected to the sender */
    void myDistanceBroadcastFunction(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    void myDistanceAcknowledgeFunction(std::shared_ptr<Message> _msg, P2PNetworkInterface* sender);

    void myLeaderBroadcastFunction(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    bool containsPosition(vector<Cell3DPosition> vectPos, Cell3DPosition pos);

    /// Advanced blockcode handlers below

    /**
     * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from it.
     * @param config : pointer to the TiXmlDocument representing the configuration file, all information related to VisibleSim's core have already been parsed
     *
     * Called from BuildingBlock constructor, only once.
     */
    void parseUserElements(TiXmlDocument *config) override {}

    /**
     * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from each block config. Has to be overriden in the child class.
     * @param config : pointer to the TiXmlElement representing the block configuration file, all information related to concerned block have already been parsed
     *
     */
    void parseUserBlockElements(TiXmlElement *config) override;

    /**
     * User-implemented debug function that gets called when a module is selected in the GUI
     */
    void onBlockSelected() override {}

    /**
     * User-implemented debug function that gets called when a VS_ASSERT is triggered
     * @note call is made from utils::assert_handler()
     */
    void onAssertTriggered() override {}

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


    /**
     * User-implemented keyboard handler function that gets called when
     *  a key press event could not be caught by openglViewer
     * @note call is made from GlutContext::keyboardFunc (openglViewer.h)
     */
    bool parseUserCommandLineArgument(int& argc, char **argv[]) override {}

    /**
     * Called by openglviewer during interface drawing phase, can be used by a user
     *  to draw a custom Gl string onto the bottom-left corner of the GUI
     * @note call is made from OpenGlViewer::drawFunc
     * @return a string (can be multi-line with `\n`) to display on the GUI
     */
    string onInterfaceDraw() override;

    bool CheckIfStuckOnClockwise();
    

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new ShapeReconfigurationBlockCode((HexanodesBlock*)host));
    };
/*****************************************************************************/
};

#endif /* ShapeReconfigurationBlockCode_H_ */
