#include "simpleColorCode.h"

Color myTabColors[8]={RED,ORANGE,YELLOW,GREEN,CYAN,BLUE,MAGENTA,GREY};

void SimpleColorCode::startup() {
	addMessageEventFunc(BROADCAST_MSG,_myBroadcastFunc);
	addMessageEventFunc(ACK_MSG,_myAckFunc);
//	console << "start\n";
    srand(time(NULL));
    nbWaitedAnswers=0;
    parent=NULL;
	if (module->blockId==1) { // master id is 1
        setColor(RED);
        distance=0;
//        nbWaitedAnswers = sendMessageToAllNeighbors("Broadcast",new MessageOf<int>(BROADCAST_MSG,distance),100,200,0);
        nbWaitedAnswers = sendMessageToAllNeighbors(new MessageOf<int>(BROADCAST_MSG,distance),100,200,0);
	} else {
        distance=-1; // unknown
        setColor(LIGHTGREY);
	}
}

void SimpleColorCode::myBroadcastFunc(const MessageOf<int>*msg, P2PNetworkInterface*sender) {
    int d = *msg->getData()+1;
//    console << "receives d=" << d << " from " << sender->getConnectedBlockId() << "\n";
    if (distance==-1) {
        distance = d; // new distance
        parent = sender; // new parent in the spinning tree
        setColor(myTabColors[distance%8]);
        nbWaitedAnswers = sendMessageToAllNeighbors("Broadcast",new MessageOf<int>(BROADCAST_MSG,distance),100,200,1,sender);
        if (nbWaitedAnswers==0) { // lead of the spinning tree
//            sendMessage("Ack",new MessageOf<int>(ACK_MSG,distance),parent,100,200);
            sendMessage(new MessageOf<int>(ACK_MSG,distance),parent,100,200);
            console << "res;"<< module->blockId << ";" << module->position[0] << ";"  << module->position[1] << ";"  << module->position[2] << ";" << distance << "\n";
        }
    } else {
//        sendMessage("Ack",new MessageOf<int>(ACK_MSG,distance),sender,100,200);
        sendMessage(new MessageOf<int>(ACK_MSG,distance),sender,100,200);
    }
};

void SimpleColorCode::myAckFunc(const MessageOf<int>*msg, P2PNetworkInterface*sender) {
    int d = *msg->getData()+1;
//    console << "receives. ACK d=" << d << " from " << sender->getConnectedBlockId() << "\n";
    if (d<distance) {
        distance=d; // update distance
        setColor(myTabColors[distance%8]);
    }
    nbWaitedAnswers--;
    if (nbWaitedAnswers==0) { // lead of the spinning tree
        console << "res;"<< module->blockId << ";" << module->position[0] << ";"  << module->position[1] << ";"  << module->position[2] << ";" << distance << "\n";
        if (parent) {
//            sendMessage("Ack",new MessageOf<int>(ACK_MSG,distance),parent,100,200);
            sendMessage(new MessageOf<int>(ACK_MSG,distance),parent,100,200);
        }
    }
};


void _myBroadcastFunc(RobotBlocksGeneric *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	SimpleColorCode *cb = (SimpleColorCode*)codebloc;
	MessageOf<int>*msgType = (MessageOf<int>*)msg.get();
	cb->myBroadcastFunc(msgType,sender);
}

void _myAckFunc(RobotBlocksGeneric *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	SimpleColorCode *cb = (SimpleColorCode*)codebloc;
	MessageOf<int>*msgType = (MessageOf<int>*)msg.get();
	cb->myAckFunc(msgType,sender);
}

