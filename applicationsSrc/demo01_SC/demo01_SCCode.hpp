#ifndef demo01_SCCode_H_
#define demo01_SCCode_H_

#include "robots/slidingCubes/slidingCubesSimulator.h"
#include "robots/slidingCubes/slidingCubesWorld.h"
#include "robots/slidingCubes/slidingCubesBlockCode.h"

static const int BROADCAST_MSG_ID = 1001;

using namespace SlidingCubes;

class Demo01_SCCode : public SlidingCubesBlockCode {
private:
	SlidingCubesBlock *module = nullptr;
public :
	Demo01_SCCode(SlidingCubesBlock *host);
	~Demo01_SCCode() {};

/**
  * This function is called on startup of the blockCode, it can be used to perform initial
  *  configuration of the host or this instance of the program.
  * @note this can be thought of as the main function of the module
  */
    void startup() override;

/**
  * @brief Message handler for the message 'broadcast'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
   void myBroadcastFunc(std::shared_ptr<Message>_msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
	    return(new Demo01_SCCode((SlidingCubesBlock*)host));
	}
/*****************************************************************************/
};

#endif /* demo01_SCCode_H_ */