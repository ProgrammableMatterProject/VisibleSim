#include "simpleColorCodeSB.h"

Color myTabColors[8]={RED,ORANGE,YELLOW,GREEN,CYAN,BLUE,MAGENTA,GREY};

void SimpleColorCode::startup() {
    addMessageEventFunc2(BROADCAST_MSG,
                         std::bind(&SimpleColorCode::myBroadcastFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));
    console << "start\n";
    srand(time(NULL));
    if (hostBlock->blockId==1) { // master id is 1
        hostBlock->setColor(RED);
        distance=0;
        sendMessageToAllNeighbors("Broadcast",new MessageOf<int>(BROADCAST_MSG,distance),100,200,0);
    } else {
        distance=-1; // unknown
        hostBlock->setColor(LIGHTGREY);
    }
}

void SimpleColorCode::myBroadcastFunc(const std::shared_ptr<Message> _msg, P2PNetworkInterface*sender) {
    MessageOf<int>*msg = (MessageOf<int>*)_msg.get();
    int d = *msg->getData()+1;
    console << "receives d=" << d << " from " << sender->getConnectedBlockId() << "\n";
    if (distance==-1 || distance>d) {
        console << "update distance=" << d << "\n";
        distance = d;
        hostBlock->setColor(myTabColors[distance%8]);
        sendMessageToAllNeighbors("Broadcast",new MessageOf<int>(BROADCAST_MSG,distance),100,200,1,sender);
    }
};
