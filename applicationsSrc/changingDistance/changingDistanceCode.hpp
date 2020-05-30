#ifndef changingDistanceCode_H_
#define changingDistanceCode_H_

#include "robots/blinkyBlocks/blinkyBlocksSimulator.h"
#include "robots/blinkyBlocks/blinkyBlocksWorld.h"
#include "robots/blinkyBlocks/blinkyBlocksBlockCode.h"
#include <limits>

static const int DISTANCEMESSAGE_MSG_ID = 1001;
static const int UPDATEMESSAGE_MSG_ID = 1002;
static const int REMOVEMESSAGE_MSG_ID = 1003;
static const int MAXINT = std::numeric_limits<int>::max() -1 ;

using namespace BlinkyBlocks;

class ChangingDistanceCode : public BlinkyBlocksBlockCode {
private:
	BlinkyBlocksBlock *module = nullptr;
  int distance = MAXINT;
  int nb_update = 0;

public :
	ChangingDistanceCode(BlinkyBlocksBlock *host);
	~ChangingDistanceCode() {};

/**
  * This function is called on startup of the blockCode, it can be used to perform initial
  *  configuration of the host or this instance of the program.
  * @note this can be thought of as the main function of the module
  */
    void startup() override;

/**
  * @brief Message handler for the message 'distanceMessage'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
   void myDistanceMessageFunc(std::shared_ptr<Message>_msg,P2PNetworkInterface *sender);

/**
   * @brief This function is called when a module is tapped by the user. Prints a message to the console by default.
   Can be overloaded in the user blockCode
    * @param face face that has been tapped */
  void onTap(int face) override;

/**
  * @brief Message handler for the message 'updateMessage'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
   void myUpdateMessageFunc(std::shared_ptr<Message>_msg,P2PNetworkInterface *sender);
/**
  * @brief Message handler for the message 'removeMessage'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
   void myRemoveMessageFunc(std::shared_ptr<Message>_msg,P2PNetworkInterface *sender);

/**
  * @brief Handler for all events received by the host block
  * @param pev pointer to the received event
  */
    void processLocalEvent(std::shared_ptr<Event> pev) override;

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
	    return(new ChangingDistanceCode((BlinkyBlocksBlock*)host));
	};
/*****************************************************************************/
};

#endif /* changingDistanceCode_H_ */