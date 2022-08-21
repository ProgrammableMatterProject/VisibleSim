#include "demo01_SCCode.hpp"

Demo01_SCCode::Demo01_SCCode(SlidingCubesBlock *host):SlidingCubesBlockCode(host),module(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;

    // Registers a callback (myBroadcastFunc) to the message of type R
    addMessageEventFunc2(BROADCAST_MSG_ID,
                       std::bind(&Demo01_SCCode::myBroadcastFunc,this,
                       std::placeholders::_1, std::placeholders::_2));

}

void Demo01_SCCode::startup() {
    console << "start " << module->blockId << "\n";
    if (module->blockId == 1) { // code for module#1 only
        setColor(RED);
        auto connectors = getAllConnectedInterfaces();
        if (connectors.find(SCLattice2::PlusX)!=connectors.end()) { // if connected send PURPLE color
            sendMessage(new MessageOf<int>(BROADCAST_MSG_ID, 18), connectors[SCLattice2::PlusX], 1000, 0);
        }
	} else if (module->blockId == 3) { // code for module#3 only
        setColor(WHITE);
        auto connectors = getAllConnectedInterfaces();
        int i=1;
        for (auto p:connectors) {
            sendMessage(new MessageOf<int>(BROADCAST_MSG_ID, i++), p.second, 1000, 0);
        }
    }
}

void Demo01_SCCode::myBroadcastFunc(std::shared_ptr<Message>_msg, P2PNetworkInterface*sender) {

    MessageOf<int>* msg = static_cast<MessageOf<int>*>(_msg.get());
    int msgData = *msg->getData();
    setColor(msgData);
}

