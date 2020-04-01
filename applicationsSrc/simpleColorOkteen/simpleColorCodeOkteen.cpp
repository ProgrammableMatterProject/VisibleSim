#include "simpleColorCodeOkteen.h"

Color myTabColors[8]={RED,ORANGE,YELLOW,GREEN,CYAN,BLUE,MAGENTA,GREY};

void SimpleColorCode::startup() {
    addMessageEventFunc(BROADCAST_MSG,_myBroadcastFunc);
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

void SimpleColorCode::myBroadcastFunc(const MessageOf<int>*msg, P2PNetworkInterface*sender) {
    int d = *msg->getData()+1;
    console << "receives d=" << d << " from " << sender->getConnectedBlockId() << "\n";
    if (distance==-1 || distance>d) {
        console << "update distance=" << d << "\n";
        distance = d;
        hostBlock->setColor(myTabColors[distance%8]);
        sendMessageToAllNeighbors("Broadcast",new MessageOf<int>(BROADCAST_MSG,distance),100,200,1,sender);
    }
};

void _myBroadcastFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    SimpleColorCode *cb = (SimpleColorCode*)codebloc;
    MessageOf<int>*msgType = (MessageOf<int>*)msg.get();
    cb->myBroadcastFunc(msgType,sender);
}
