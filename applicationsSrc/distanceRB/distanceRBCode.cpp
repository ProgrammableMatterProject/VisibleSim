#include "distanceRBCode.h"

void DistanceRBCode::startup() {
	addMessageEventFunc(BROADCAST_MSG,_myBROADCAST_MSGFunc);
	console << "start " << module->blockId << "\n";
	if (module->blockId==1) { // master id is 1
		distance=0;
		sendMessageToAllNeighbors(new MessageOf<uint16_t>(BROADCAST_MSG,0),200,50,0);
		setColor(RED);
	} else {
		distance = UINT16_MAX;
		setColor(LIGHTGREY);
	}
}

void DistanceRBCode::myBROADCAST_MSGFunc(const MessageOf<uint16_t>*msg, P2PNetworkInterface*sender) {
	uint16_t d = *msg->getData()+1;
	console << "rec. d=" << d << " from " << sender->getConnectedBlockId() << "\n";
	if (d<distance) {
		distance = d;
		setColor(Colors[distance%9]);
		sendMessageToAllNeighbors(new MessageOf<uint16_t>(BROADCAST_MSG,distance),200,50,1,sender);
	}

};

void _myBROADCAST_MSGFunc(GenericCodeBlock *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	DistanceRBCode *cb = (DistanceRBCode*)codebloc;
	MessageOf<uint16_t>*msgType = (MessageOf<uint16_t>*)msg.get();
	cb->myBROADCAST_MSGFunc(msgType,sender);
}
