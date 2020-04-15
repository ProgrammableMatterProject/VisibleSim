#include "sinkSourceCode.h"

Color myTabColors[8]={RED,ORANGE,YELLOW,GREEN,CYAN,BLUE,MAGENTA,GREY};

void SinkSourceCode::startup() {
    addMessageEventFunc2(BROADCAST_MSG,
                         std::bind(&SinkSourceCode::myBroadcastFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));
    addMessageEventFunc2(DISTANCE_MSG,
                         std::bind(&SinkSourceCode::myDistanceFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));
    addMessageEventFunc2(PROPOSE_MSG,
                         std::bind(&SinkSourceCode::myProposeFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));
    addMessageEventFunc2(ANSWER_MSG,
                         std::bind(&SinkSourceCode::myAnswerFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));
    addMessageEventFunc2(MOVE_MSG,
                         std::bind(&SinkSourceCode::myMoveFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    console << "start\n";
    srand(time(NULL));
        wait = false;
        proposePath = NULL;
        answerPath = NULL;
        movePath = NULL;
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

void SinkSourceCode::myBroadcastFunc(const std::shared_ptr<Message> _msg, P2PNetworkInterface*sender) {
    MessageOf<int>*msg = (MessageOf<int>*)_msg.get();
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

void SinkSourceCode::myDistanceFunc(const std::shared_ptr<Message> _msg, P2PNetworkInterface*sender) {
    MessageOf<int>*msg = (MessageOf<int>*)_msg.get();
    int d = *msg->getData()+1;
/*    if (distance==0){
    sendMessageToAllNeighbors("Distance",new MessageOf<int>(DISTANCE_MSG,distance),100,200,0);
    }*/
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

void SinkSourceCode::myProposeFunc(const std::shared_ptr<Message> _msg, P2PNetworkInterface*sender) {
    MessageOf<int>*msg = (MessageOf<int>*)_msg.get();
    int proposed = *msg->getData();
    if (proposePath == NULL) {
        int taken = min(abs(value),proposed);
        //value += taken;
        sendMessage("Answer",new MessageOf<int>(ANSWER_MSG,taken),sender,100,200);
    }
    else {
        answerPath = sender;
        sendMessage("Propose",new MessageOf<int>(PROPOSE_MSG,proposed),proposePath,100,200);
        proposePath = NULL;
    }
/*    if (value < 0){
        distance = 0;
        //hostBlock->setColor(RED);
    //((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
    }
    else if (value > 0){
        distance = -1;
    //hostBlock->setColor(GREEN);
    //((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
    }
    else if (value == 0){
        distance = -1;
    //hostBlock->setColor(ORANGE);
    //((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
    }*/
};

void SinkSourceCode::myAnswerFunc(const std::shared_ptr<Message> _msg, P2PNetworkInterface*sender) {
    MessageOf<int>*msg = (MessageOf<int>*)_msg.get();
    int taken = *msg->getData();
    if (answerPath == NULL){
        value -= taken;
        distance = -1;
        wait = false;
        sendMessage("Move",new MessageOf<int>(MOVE_MSG,taken),sender,100,200);
    }
    else {
        movePath = sender;
        sendMessage("Answer",new MessageOf<int>(ANSWER_MSG,taken),answerPath,100,200);
        answerPath = NULL;
    }
    if (value < 0){
        hostBlock->setColor(RED);
        //distance = 0;
    ((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
    }
    else if (value > 0){
    hostBlock->setColor(GREEN);
        //distance = -1;
    ((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
    }
    else if (value == 0){
    hostBlock->setColor(ORANGE);
        //distance = -1;
    ((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
    }
};

void SinkSourceCode::myMoveFunc(const std::shared_ptr<Message> _msg, P2PNetworkInterface*sender) {
    MessageOf<int>*msg = (MessageOf<int>*)_msg.get();
    int taken = *msg->getData();
    //value += taken;
    if (movePath == NULL){
        value += taken;
    }
    else {
        sendMessage("Move",new MessageOf<int>(MOVE_MSG,taken),movePath,100,200);        movePath = NULL;
    }
    if (value < 0){
        hostBlock->setColor(RED);
    ((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
        distance = 0;
    sendMessageToAllNeighbors("Distance",new MessageOf<int>(DISTANCE_MSG,distance),100,200,0);
    }
    else if (value > 0){
    hostBlock->setColor(GREEN);
    ((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
        distance = -1;
    }
    else if (value == 0){
    hostBlock->setColor(ORANGE);
    ((SmartBlocksGlBlock*)(hostBlock->ptrGlBlock))->setDisplayedValue(abs(value));
        distance = -1;
    }
};
