#ifndef StressTestSBBlockCode_H_
#define StressTestSBBlockCode_H_

#include "robots/smartBlocks/smartBlocksWorld.h"
#include "robots/smartBlocks/smartBlocksBlockCode.h"

static const int ACTIVATION_MSG_ID = 0x01;
static const int AGITATE_IT_ID = 0x01;

using namespace SmartBlocks;

class StressTestSBBlockCode : public SmartBlocksBlockCode {
private:
    Cell3DPosition lastPos;
    Lattice *lattice;
    bool activated = false;
		bool isMoving=false;
		bool isLeader=false;
    SmartBlocksBlock *module;
    inline static bool *lockedCells  = nullptr;
    std::mt19937 rng;
public :
    StressTestSBBlockCode(SmartBlocksBlock *host);
    ~StressTestSBBlockCode() {
        if (lockedCells) {
            delete [] lockedCells;
            lockedCells = nullptr;
        }
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
    void processLocalEvent(std::shared_ptr<Event> pev) override;

    /**
     * @brief Callback function executed whenever the module finishes a motion
     */
    void onMotionEnd() override;
		
		/**
		 * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from each block config. Has to be overriden in the child class.
		 * @param config : pointer to the TiXmlElement representing the block configuration file, all information related to concerned block have already been parsed
		 *
		 */
		void parseUserBlockElements(TiXmlElement *config) override;
		
    /**
     * @brief Activation message handler for this instance of the blockcode
     * @param _msg Pointer to the (abstracted) message received by the module
     * @param sender Connector of the module that has received the message and that is connected to the sender */
    void handleActivationMessage(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    void initLockedCells();
    inline bool isLockedCell(const Cell3DPosition& pos) const {
        return lockedCells[lattice->getIndex(pos)];
    }
    inline void setLockedCell(const Cell3DPosition& pos, bool s) {
        lockedCells[lattice->getIndex(pos)] = s;
    }

    void wait();
    bool randomWalk();

    /// Advanced blockcode handlers below

    /**
     * User-implemented debug function that gets called when a module is selected in the GUI
     */
    void onBlockSelected() override;

    /**
     * User-implemented debug function that gets called when a VS_ASSERT is triggered
     * @note call is made from utils::assert_handler()
     */
    void onAssertTriggered() override {};

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
    void onGlDraw() override;

    /**
     * @brief This function is called when a module is tapped by the user. Prints a message to the console by default.
     Can be overloaded in the user blockCode
     * @param face face that has been tapped */
    void onTap(int face) override {}

    bool parseUserCommandLineArgument(int& argc, char **argv[]) override { return false; };
		string onInterfaceDraw() override;
/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new StressTestSBBlockCode((SmartBlocksBlock*)host));
    };
/*****************************************************************************/
};

#endif /* StressTestSBBlockCode_H_ */
