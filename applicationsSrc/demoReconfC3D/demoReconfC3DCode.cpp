#include <climits>
#include "demoReconfC3DCode.h"

static const Cell3DPosition firstCell(4,2,0);

DemoReconfCode::DemoReconfCode(Catoms3DBlock *host):Catoms3DBlockCode(host),module(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;

    // Registers a callback (myBroadcastFunc) to the message of type R
    addMessageEventFunc2(GO_MSG_ID,
                         std::bind(&DemoReconfCode::myGoFunc,this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myForecastFunc) to the message of type R
    addMessageEventFunc2(BACK_MSG_ID,
                         std::bind(&DemoReconfCode::myBackFunc,this,
                                   std::placeholders::_1, std::placeholders::_2));

    myParent=nullptr;
    myDistance=255;
    myRound=0;
}

void DemoReconfCode::startup() {
    lattice = (FCCLattice*)(Catoms3D::getWorld()->lattice);

    /*auto neibList=lattice->getNeighborhood(module->position);
    auto it = neibList.begin();
    while (it!=neibList.end() && (*it)!=firstCell) {
        it++;
    }
    if (it!=neibList.end()) {
        cout << "position:" << module->position << endl;
        module->setColor(RED);
        myNbWaitedAnswers = sendMessageToAllNeighbors("go",new MessageOf<pair<uint8_t,uint16_t>>(GO_MSG_ID,pair<uint8_t,uint16_t>(1,1)), 100, 1000,0);
    }*/
    if (getId()==29) {
        auto dest = module->getAllMotions();
        if (!dest.empty()) {
            uint8_t distmin = (dest[0].first).dist_taxi(firstCell);
            uint8_t dist,imin=0;
            cout << dest[0].first << "," << (dest[0].first).dist_taxi(firstCell) << endl;
            for (uint8_t i=1; i<dest.size(); i++) {
                cout << dest[i].first << "," << (dest[i].first).dist_taxi(firstCell) << endl;
                dist = (dest[i].first).dist_taxi(firstCell);
                if (dist<distmin) {
                    distmin=dist;
                    imin=i;
                }
            }
            module->moveTo(dest[imin].first);
        }
    }

}

bool DemoReconfCode::tryToMove() {
    bool found=true;
    return found;
}

void DemoReconfCode::onMotionEnd() {

}

void DemoReconfCode::myGoFunc(std::shared_ptr<Message>_msg, P2PNetworkInterface*sender) {
    MessageOf<pair<uint8_t,uint16_t>>* msg = static_cast<MessageOf<pair<uint8_t,uint16_t>>*>(_msg.get());
    uint8_t msgDistance = (*msg->getData()).first;
    uint16_t msgRound = (*msg->getData()).second;

    console << "rcv " << int(msgDistance) << "," << int(msgRound) << " from " << sender->getConnectedBlockBId() << "\n";
    if (myParent==nullptr || msgRound>myRound) {
        myDistance = msgDistance;
        myRound=msgRound;
        myParent=sender;
        setColor(myDistance);
        myNbWaitedAnswers=sendMessageToAllNeighbors("go",
                                                    new MessageOf<pair<uint8_t,uint16_t>>(GO_MSG_ID,pair<uint8_t,uint16_t>(msgDistance+1,msgRound)), 100, 1000, 1, sender);
        if (myNbWaitedAnswers==0) {
            auto v=module->getAllMotions();
            if (v.empty()) setColor(GREY);
            sendMessage("back",new Message(BACK_MSG_ID),myParent,100,1000);
        }
    } else {
        auto v=module->getAllMotions();
        if (v.empty()) setColor(GREY);
        sendMessage("back",new Message(BACK_MSG_ID),sender,100,1000);
    }
}

void DemoReconfCode::myBackFunc(std::shared_ptr<Message>_msg, P2PNetworkInterface*sender) {
    console << "rcv ACK  from " << sender->getConnectedBlockBId() << "\n";
    myNbWaitedAnswers--;
    if (myNbWaitedAnswers==0) {
        if (myParent) {
            auto v=module->getAllMotions();
            if (v.empty()) setColor(GREY);
            sendMessage("back", new Message(BACK_MSG_ID), myParent, 100, 1000);
        } else {
            setColor(BLACK);
        }
    }
}
