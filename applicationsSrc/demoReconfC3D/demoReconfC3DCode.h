#ifndef DemoReconfCode_H_
#define DemoReconfCode_H_
#include <queue>
#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DBlockCode.h"
#include "robots/catoms3D/catoms3DMotionEngine.h"
#include "robots/catoms3D/catoms3DRotationEvents.h"
#include "motion/teleportationEvents.h"

static const int GO_MSG_ID = 1001;
static const int BACK_MSG_ID = 1002;

using namespace Catoms3D;

class DemoReconfCode : public Catoms3DBlockCode {
private:
    Catoms3DBlock *module;
    FCCLattice *lattice;
    uint8_t myNbWaitedAnswers;
    uint8_t myDistance;
    uint16_t myRound;
    P2PNetworkInterface *myParent;
public :
    DemoReconfCode(Catoms3DBlock *host);
    ~DemoReconfCode() {};

    void startup() override;
    bool tryToMove();

    /**
  * @brief Message handler for the message 'Go'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myGoFunc(std::shared_ptr<Message>_msg,P2PNetworkInterface *sender);

/**
  * @brief Message handler for the message 'Back'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myBackFunc(std::shared_ptr<Message>_msg,P2PNetworkInterface *sender);

    void onMotionEnd() override;
    /*****************************************************************************/
    /** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return(new DemoReconfCode((Catoms3DBlock*)host));
    };
    /*****************************************************************************/
};

#endif /* DemoReconfCode_H_ */
