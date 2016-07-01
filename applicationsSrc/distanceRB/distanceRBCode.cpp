#include "distanceRBCode.h"

void DistanceRBCode::startup() {
	addMessageEventFunc(BROADCAST_MSG,_myBROADCAST_MSGFunc);
	console << "start " << module->blockId << '\n';
	currentLevel=0;
	if (module->blockId==1) { // master id is 1
		distance=0;
		setColor(RED);
		sendMessageToAllNeighbors("Broadcast",new MessageOf<DistanceMessage>(BROADCAST_MSG,DistanceMessage(0,1)),200,50,0);
	} else {
		distance=UINT16_MAX;
		setColor(LIGHTGREY);
	}
}

void DistanceRBCode::myBROADCAST_MSGFunc(const MessageOf<DistanceMessage>*msg, P2PNetworkInterface*sender) {
	DistanceMessage *dm = msg->getData();
	uint16_t d = dm->distance+1;
	uint8_t level = dm->level;

	console << "rec. d=" << d << " from " << sender->getConnectedBlockId() << "\n";
	if (distance>d || level!=currentLevel) {
		distance = d;
		currentLevel = level;
		setColor(Colors[distance % 9]);
		sendMessageToAllNeighbors("Broadcast",new MessageOf<DistanceMessage>(BROADCAST_MSG,DistanceMessage(distance,level)),200,50,1,sender);
	}
};

void DistanceRBCode::onTap() {
	distance=0;
	setColor(RED);
	currentLevel = currentLevel+1;
	sendMessageToAllNeighbors("Broadcast",new MessageOf<DistanceMessage>(BROADCAST_MSG,DistanceMessage(0,currentLevel)),200,50,0);
}

void _myBROADCAST_MSGFunc(GenericCodeBlock *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	DistanceRBCode *cb = (DistanceRBCode*)codebloc;
	MessageOf<DistanceMessage>*msgType = (MessageOf<DistanceMessage>*)msg.get();
	cb->myBROADCAST_MSGFunc(msgType,sender);
}
