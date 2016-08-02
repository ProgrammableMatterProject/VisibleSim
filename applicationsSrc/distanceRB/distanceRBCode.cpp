#include "distanceRBCode.h"

void DistanceRBCode::startup() {
	addMessageEventFunc(BROADCAST_MSG,_myBROADCAST_MSGFunc);
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

void DistanceRBCode::myBROADCAST_MSGFunc(const MessageOf<uint16_t>*msg, P2PNetworkInterface*sender) {
	uint16_t d = *(msg->getData())+1;
	console << "rec." << d << " from " << sender->getConnectedBlockId() << "\n";
	if (distance>d) {
		distance=d;
		hostBlock->setColor(Colors[distance%9]);
		sendMessageToAllNeighbors("broadcast",new MessageOf<uint16_t>(BROADCAST_MSG,distance),2000,100,1,sender);
	}
};

void _myBROADCAST_MSGFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	DistanceRBCode *cb = (DistanceRBCode*)codebloc;
	MessageOf<uint16_t>*msgType = (MessageOf<uint16_t>*)msg.get();
	cb->myBROADCAST_MSGFunc(msgType,sender);
}
