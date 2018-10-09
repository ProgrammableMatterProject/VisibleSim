#include "sinkSourceCode.h"

Color myTabColors[8]={RED,ORANGE,YELLOW,GREEN,CYAN,BLUE,MAGENTA,GREY};

void SinkSourceCode::startup() {
	addMessageEventFunc(BROADCAST_MSG,_myBroadcastFunc);
	addMessageEventFunc(DISTANCE_MSG,_myDistanceFunc);
	addMessageEventFunc(PROPOSE_MSG,_myProposeFunc);
	addMessageEventFunc(ANSWER_MSG,_myAnswerFunc);
	console << "start\n";
    srand(time(NULL));
        wait = false;
        proposePath = NULL;
        answerPath = NULL;
	if (hostBlock->blockId==1) { // master id is 1
        //hostBlock->setColor(RED);
        //distance=0;
	value = -1;
        distance=0;
	//((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(0);
        //sendMessageToAllNeighbors("Broadcast",new MessageOf<int>(BROADCAST_MSG,distance),100,200,0);
	}
	else if (hostBlock->blockId==2) {
	value = -2;
        distance=0;
	}
	else if (hostBlock->blockId==3) {
	value = -3;
        distance=0;
	}
	else if (hostBlock->blockId==4) {
	value = -4;
        distance=0;
	}
	else if (hostBlock->blockId==5) {
	value = -5;
        distance=0;
	}
	else if (hostBlock->blockId==6) {
	value = -6;
        distance=0;
	}
	else if (hostBlock->blockId==7) {
	value = -7;
        distance=0;
	}
	else if (hostBlock->blockId==8) {
	value = -8;
        distance=0;
	}
	else if (hostBlock->blockId==9) {
	value = -9;
        distance=0;
	}
	else if (hostBlock->blockId==72) {
	value = 1;
        distance=-1; // unknown
	}
	else if (hostBlock->blockId==71) {
	value = 2;
        distance=-1; // unknown
	}
	else if (hostBlock->blockId==70) {
	value = 3;
        distance=-1; // unknown
	}
	else if (hostBlock->blockId==69) {
	value = 4;
        distance=-1; // unknown
	}
	else if (hostBlock->blockId==68) {
	value = 5;
        distance=-1; // unknown
	}
	else if (hostBlock->blockId==67) {
	value = 6;
        distance=-1; // unknown
	}
	else if (hostBlock->blockId==66) {
	value = 7;
        distance=-1; // unknown
	}
	else if (hostBlock->blockId==65) {
	value = 8;
        distance=-1; // unknown
	}
	else if (hostBlock->blockId==64) {
	value = 9;
        distance=-1; // unknown
	}
	else {
        distance=-1; // unknown
	value = 0;
        //hostBlock->setColor(LIGHTGREY);
	}
	if (value < 0){
	hostBlock->setColor(RED);
	((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
	sendMessageToAllNeighbors("Distance",new MessageOf<int>(DISTANCE_MSG,distance),100,200,0);
	}
	else if (value > 0){
	hostBlock->setColor(GREEN);
	((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
	}
	else if (value == 0){
	hostBlock->setColor(ORANGE);
	((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
	}
}

void SinkSourceCode::myBroadcastFunc(const MessageOf<int>*msg, P2PNetworkInterface*sender) {
    int d = *msg->getData()+1;
    console << "receives d=" << d << " from " << sender->getConnectedBlockId() << "\n";
    if (distance==-1 || distance>d) {
        console << "update distance=" << d << "\n";
        distance = d;
		((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(distance);
        hostBlock->setColor(myTabColors[distance%8]);
        sendMessageToAllNeighbors("Broadcast",new MessageOf<int>(BROADCAST_MSG,distance),100,200,1,sender);
    }
};

void SinkSourceCode::myDistanceFunc(const MessageOf<int>*msg, P2PNetworkInterface*sender) {
    int d = *msg->getData()+1;
    if (distance==-1 || distance>d) {
        distance = d;
        if (!wait){
            if (value == 0){
                proposePath = sender;
                sendMessageToAllNeighbors("Distance",new MessageOf<int>(DISTANCE_MSG,distance),100,200,1,sender);
            }
            if (value > 0){
                wait=true;
                hostBlock->setColor(BLUE);
                sendMessage("Propose",new MessageOf<int>(PROPOSE_MSG,value),sender,100,200);
            }
        }
    }
};

void SinkSourceCode::myProposeFunc(const MessageOf<int>*msg, P2PNetworkInterface*sender) {
    int proposed = *msg->getData();
    if (proposePath == NULL) {
        int taken = min(abs(value),proposed);
        value += taken;
        sendMessage("Answer",new MessageOf<int>(ANSWER_MSG,taken),sender,100,200);
    }
    else {
        answerPath = sender;
        sendMessage("Propose",new MessageOf<int>(PROPOSE_MSG,proposed),proposePath,100,200);
        proposePath = NULL;
    }
    if (value < 0){
        hostBlock->setColor(RED);
	((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
	}
    else if (value > 0){
	hostBlock->setColor(GREEN);
	((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
	}
    else if (value == 0){
	hostBlock->setColor(ORANGE);
	((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
	}
};

void SinkSourceCode::myAnswerFunc(const MessageOf<int>*msg, P2PNetworkInterface*sender) {
    int taken = *msg->getData();
    if (answerPath == NULL){
        value -= taken;
        wait = false;
    }
    else {
        sendMessage("Answer",new MessageOf<int>(ANSWER_MSG,taken),answerPath,100,200);
        answerPath = NULL;
    }
    if (value < 0){
        hostBlock->setColor(RED);
	((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
	}
    else if (value > 0){
	hostBlock->setColor(GREEN);
	((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
	}
    else if (value == 0){
	hostBlock->setColor(ORANGE);
	((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
	}
};

void _myBroadcastFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	SinkSourceCode *cb = (SinkSourceCode*)codebloc;
	MessageOf<int>*msgType = (MessageOf<int>*)msg.get();
	cb->myBroadcastFunc(msgType,sender);
}

void _myDistanceFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	SinkSourceCode *cb = (SinkSourceCode*)codebloc;
	MessageOf<int>*msgType = (MessageOf<int>*)msg.get();
	cb->myDistanceFunc(msgType,sender);
}

void _myProposeFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	SinkSourceCode *cb = (SinkSourceCode*)codebloc;
	MessageOf<int>*msgType = (MessageOf<int>*)msg.get();
	cb->myProposeFunc(msgType,sender);
}

void _myAnswerFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	SinkSourceCode *cb = (SinkSourceCode*)codebloc;
	MessageOf<int>*msgType = (MessageOf<int>*)msg.get();
	cb->myAnswerFunc(msgType,sender);
}
