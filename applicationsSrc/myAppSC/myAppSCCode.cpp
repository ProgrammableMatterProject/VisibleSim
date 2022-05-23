#include "myAppSCCode.hpp"

MyAppSCCode::MyAppSCCode(SlidingCubesBlock *host) : SlidingCubesBlockCode(host), module(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;
    addMessageEventFunc2(ELECT_MSG_ID,
                         std::bind(&MyAppSCCode::myElectFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

}

void MyAppSCCode::startup() {
    console << "start " << module->blockId << "\n";
    if (isLeader) { // Master id is 1
        module->setColor(RED);
        tryToMove();
    }
}

void MyAppSCCode::myElectFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender) {
    console << "rec. Elect() from " << sender->getConnectedBlockId() << "\n";
    // search a neighbor in (x,y-1,z)
    module->setColor(YELLOW);
    Cell3DPosition prevPos(module->position[0], module->position[1] - 1, module->position[2]);
    auto p2p=findNeighborAt(prevPos);
    if (p2p) {
        module->setColor(GREEN);
        sendMessage(new Message(ELECT_MSG_ID), p2p, 200000, 0);
    } else {
        // if not try to move
        if (!tryToMove()) {
            console << "the end!\n";
        }
    }
}

P2PNetworkInterface *MyAppSCCode::findNeighborAt(const Cell3DPosition &pos) {
    int i = 0;
    while (i<6 && (!module->getInterface(SCLattice::Direction(i))->isConnected() ||
           module->getInterface(SCLattice::Direction(i))->connectedInterface->hostBlock->position != pos)) {
        i++;
    }
    cout << "found=" << i << endl;
    if (i < 6) {
        return module->getInterface(SCLattice::Direction(i));
    }
    return nullptr;
}


void MyAppSCCode::parseUserBlockElements(TiXmlElement *config) {
    const char *attr = config->Attribute("leader");
    if (attr != nullptr) {
        string s(attr);
        isLeader = (s == "1" || s == "yes" || s == "true");
        if (isLeader) std::cout << module->blockId << " is leader!" << std::endl; // complete with your code
    }
}

void MyAppSCCode::onMotionEnd() {
    // complete with your code
    console << " End of motion to " << module->position << "\n";
    /*if (!tryToMove()) {
        // search a neighbor in (x,y-1,z)
        Cell3DPosition prevPos(module->position[0], module->position[1] - 1, module->position[2]);
        auto p2p=findNeighborAt(prevPos);
        if (p2p) {
            module->setColor(GREEN);
            sendMessage(new Message(ELECT_MSG_ID), p2p, 200000, 0);
        }
    }*/
}

bool MyAppSCCode::tryToMove() {
    auto posLeft = module->position + Cell3DPosition(0, 1, 0);
    cout << "try " << posLeft << endl;
    if (module->canMoveTo(posLeft)) {
        module->moveTo(posLeft);
        return true;
    }
    posLeft = module->position + Cell3DPosition(0, 1, 1);
    cout << "try " << posLeft << endl;
    if (module->canMoveTo(posLeft)) {
        module->moveTo(posLeft);
        return true;
    }
    posLeft = module->position + Cell3DPosition(0, 1, -1);
    cout << "try " << posLeft << endl;
    if (module->canMoveTo(posLeft)) {
        module->moveTo(posLeft);
        return true;
    }
    return false;
}
