#ifndef rainbowCode_H_
#define rainbowCode_H_

#include "robots/blinkyBlocks/blinkyBlocksSimulator.h"
#include "robots/blinkyBlocks/blinkyBlocksWorld.h"
#include "robots/blinkyBlocks/blinkyBlocksBlockCode.h"

static const int LAYERMESSAGE_MSG_ID = 1001;
static const int UPDATEMESSAGE_MSG_ID = 1002;

using namespace BlinkyBlocks;

class RainbowCode : public BlinkyBlocksBlockCode
{
private:
  BlinkyBlocksBlock *module = nullptr;
  int layer = 0;
  std::array<P2PNetworkInterface*,6> listItf;
  int nb_update = 0 ;

public:
  RainbowCode(BlinkyBlocksBlock *host);
  ~RainbowCode(){};

  /**
  * This function is called on startup of the blockCode, it can be used to perform initial
  *  configuration of the host or this instance of the program.
  * @note this can be thought of as the main function of the module
  */
  void startup() override;

  /**
  * @brief Message handler for the message 'layerMessage'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
  void myLayerMessageFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

/**
  * @brief Message handler for the message 'layerMessage'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
  void myUpdateMessageFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);
  /**
  * @brief Message sender with the layer of the BB
  */
  void sendLayer();
/**
 * @brief Handler for all events received by the host block
 */
  void processLocalEvent(EventPtr pev) override;
  /*****************************************************************************/
  /** needed to associate code to module                                      **/
  static BlockCode *buildNewBlockCode(BuildingBlock *host)
  {
    return (new RainbowCode((BlinkyBlocksBlock *)host));
  };
  /*****************************************************************************/
};

#endif /* rainbowCode_H_ */