#include "distanceSCCode.h"

void DistanceSCCode::startup() {
    addMessageEventFunc2(BROADCAST_MSG,
                         std::bind(&DistanceSCCode::myBROADCAST_MSGFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));
    console << "start " << hostBlock->blockId << "\n";
    if (hostBlock->blockId==1) { // master id is 1
        distance=0;
        hostBlock->setColor(RED);
        sendMessageToAllNeighbors("broadcast",new MessageOf<uint16_t>(BROADCAST_MSG,0),2000,100,0);
    } else {
        distance = UINT16_MAX;
        hostBlock->setColor(LIGHTGREY);
    }
}

void DistanceSCCode::myBROADCAST_MSGFunc(const std::shared_ptr<Message> _msg, P2PNetworkInterface*sender) {
    MessageOf<uint16_t>*msg = (MessageOf<uint16_t>*)_msg.get();
    uint16_t d = *(msg->getData())+1;
    console << "rec." << d << " from " << sender->getConnectedBlockId() << "\n";
    if (distance>d) {
        distance=d;
        hostBlock->setColor(Colors[distance%9]);
        sendMessageToAllNeighbors("broadcast",new MessageOf<uint16_t>(BROADCAST_MSG,distance),2000,100,1,sender);
    }
};
