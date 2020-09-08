#ifndef comeBackCode_H_
#define comeBackCode_H_

#include "robots/slidingCubes/slidingCubesSimulator.h"
#include "robots/slidingCubes/slidingCubesWorld.h"
#include "robots/slidingCubes/slidingCubesBlockCode.h"

static const int SENDDISTANCE_MSG_ID = 1001;
static const int ANSWERDISTANCE_MSG_ID = 1002;
static const int ASKDISTANCE_MSG_ID = 1003;
static const int ONPATH_MSG_ID = 1004;

using namespace SlidingCubes;

class ComeBackCode : public SlidingCubesBlockCode {
private:
	SlidingCubesBlock *module = nullptr;
    int distance;
    bool isTarget = false;
    bool targetSent = false;
    bool isWall = false;
    bool isPath = false;
public :
	ComeBackCode(SlidingCubesBlock *host);
	~ComeBackCode() {};

/**
  * This function is called on startup of the blockCode, it can be used to perform initial
  *  configuration of the host or this instance of the program.
  * @note this can be thought of as the main function of the module
  */
    void startup() override;

/**
  * @brief Message handler for the message 'sendDistance'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
   void mySendDistanceFunc(std::shared_ptr<Message>_msg,P2PNetworkInterface *sender);

/**
  * @brief Message handler for the message 'answerDistance'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
   void myAnswerDistanceFunc(std::shared_ptr<Message>_msg,P2PNetworkInterface *sender);

/**
  * @brief Message handler for the message 'askDistance'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
   void myAskDistanceFunc(std::shared_ptr<Message>_msg,P2PNetworkInterface *sender);

/**
  * @brief Message handler for the message 'onPath'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
   void myOnPathFunc(std::shared_ptr<Message>_msg,P2PNetworkInterface *sender);

    void parseUserBlockElements(TiXmlElement *blockElt) override;
/**
  * @brief Handler for all events received by the host block
  * @param pev pointer to the received event
  */
    void processLocalEvent(std::shared_ptr<Event> pev) override;

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
	    return(new ComeBackCode((SlidingCubesBlock*)host));
	};
/*****************************************************************************/
};

#endif /* comeBackCode_H_ */