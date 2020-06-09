#include "stressTestCatomsCode.hpp"

StressTestCatomsCode::StressTestCatomsCode(Catoms3DBlock *host):Catoms3DBlockCode(host),module(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;
    // Registers a callback (myActivateFunc) to the message of type C
    addMessageEventFunc2(ACTIVATE_MSG_ID,
                         std::bind(&StressTestCatomsCode::myActivateFunc,this,
                                   std::placeholders::_1, std::placeholders::_2));
}

void StressTestCatomsCode::startup() {
    if (isRemovable()) {
      module->setColor(RED);
      remove();
    }
}

void StressTestCatomsCode::remove() {
    sendMessageToAllNeighbors("Activate",new Message(ACTIVATE_MSG_ID),1000,200,0);
    //module->setVisible(false);
}

void StressTestCatomsCode::myActivateFunc(std::shared_ptr<Message>_msg, P2PNetworkInterface*sender) {
    if (isRemovable()) {
        module->setColor(RED);
        //remove();
    } else {
        module->setColor(GREEN);
    }
};
/*
string StressTestCatomsCode::onInterfaceDraw() {
    return "My text\nIn two lines"; // to update with your text
};*/

bool StressTestCatomsCode::isRemovable() {
    uint8_t groups[24][6]={
      {2,3,4,5,0,1},{2,3,4,5,1,6},{2,3,4,5,6,7},{2,3,4,5,7,0},
      {0,1,2,10,3,11},{0,1,2,10,11,9},{0,1,2,10,9,5},{0,1,2,10,5,3},
      {1,6,3,11,2,4},{1,6,3,11,4,8},{1,6,3,11,8,10},{1,6,3,11,10,2},
      {6,7,4,8,3,5},{6,7,4,8,5,9},{6,7,4,8,9,11},{6,7,4,8,11,3},
      {7,0,5,9,4,2},{7,0,5,9,2,10},{7,0,5,9,10,8},{7,0,5,9,8,4},
      {10,11,8,9,1,6},{10,11,8,9,6,7},{10,11,8,9,7,0},{10,11,8,9,0,1}};
    bool test;
    int ntest = (module->position[2]!=0)?24:4;
    for (uint8_t i=0; i<ntest; i++) {
      test=false;
      for (uint8_t j=0; j<6; j++) {
        test|= module->getInterface(groups[i][j])->isConnected();
      }
      if (!test) return true;
    }
    return false;
}
