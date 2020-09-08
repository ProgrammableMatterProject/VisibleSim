#ifndef stressTestCatomsCode_H_
#define stressTestCatomsCode_H_

#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DWorld.h"
#include "robots/catoms3D/catoms3DBlockCode.h"

static const int ACTIVATE_MSG_ID = 1001;

using namespace Catoms3D;

class StressTestCatomsCode : public Catoms3DBlockCode {
private:
	Catoms3DBlock *module = nullptr;

	bool isRemovable();
	void remove();
public :
	StressTestCatomsCode(Catoms3DBlock *host);
	~StressTestCatomsCode() {};

/**
  * This function is called on startup of the blockCode, it can be used to perform initial
  *  configuration of the host or this instance of the program.
  * @note this can be thought of as the main function of the module
  */
    void startup() override;

/**
  * @brief Message handler for the message 'activate'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myActivateFunc(std::shared_ptr<Message>_msg,P2PNetworkInterface *sender);
/**
  * Called by openglviewer during interface drawing phase, can be used by a user
  *  to draw a custom Gl string onto the bottom-left corner of the GUI
  * @note call is made from OpenGlViewer::drawFunc
  * @return a string (can be multi-line with `
`) to display on the GUI

    string onInterfaceDraw() override;*/

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
	    return(new StressTestCatomsCode((Catoms3DBlock*)host));
	};
/*****************************************************************************/
};

#endif /* stressTestCatomsCode_H_ */
