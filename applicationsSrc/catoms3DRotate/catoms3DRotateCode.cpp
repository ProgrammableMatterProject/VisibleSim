#include <climits>
#include "catoms3DRotateCode.h"

/* config 3 toutes les solutions */
// # solutions: 16
vector<vector<ModuleData>> order= {{{{1,3,2},{1,2,3},0},{{3,3,2},{2,2,2},1},{{1,2,3},{1,2,2},2},{{2,3,2},{3,2,2},3}}
        ,{{{1,3,2},{1,2,3},0},{{3,3,2},{2,2,2},1},{{2,3,2},{3,2,2},2},{{1,2,3},{1,2,2},3}}
        ,{{{1,3,2},{1,2,1},0},{{3,3,2},{2,2,2},1},{{2,3,2},{3,2,2},2},{{1,2,1},{1,2,2},3}}
        ,{{{1,3,2},{2,2,2},0},{{3,3,2},{2,2,3},1},{{2,2,3},{3,2,2},2},{{2,3,2},{1,2,2},3}}
        ,{{{1,3,2},{2,2,2},0},{{3,3,2},{2,2,3},1},{{2,3,2},{1,2,2},2},{{2,2,3},{3,2,2},3}}
        ,{{{1,3,2},{2,2,2},0},{{3,3,2},{2,2,1},1},{{2,3,2},{1,2,2},2},{{2,2,1},{3,2,2},3}}
        ,{{{1,3,2},{1,2,1},0},{{3,3,2},{2,2,2},1},{{1,2,1},{1,2,2},2},{{2,3,2},{3,2,2},3}}
        ,{{{1,3,2},{2,2,2},0},{{3,3,2},{2,2,1},1},{{2,2,1},{3,2,2},2},{{2,3,2},{1,2,2},3}}
        ,{{{3,3,2},{2,2,2},0},{{1,3,2},{1,2,3},1},{{1,2,3},{1,2,2},2},{{2,3,2},{3,2,2},3}}
        ,{{{3,3,2},{2,2,2},0},{{1,3,2},{1,2,1},1},{{1,2,1},{1,2,2},2},{{2,3,2},{3,2,2},3}}
        ,{{{3,3,2},{2,2,2},0},{{1,3,2},{1,2,3},1},{{2,3,2},{3,2,2},2},{{1,2,3},{1,2,2},3}}
        ,{{{3,3,2},{2,2,2},0},{{1,3,2},{1,2,1},1},{{2,3,2},{3,2,2},2},{{1,2,1},{1,2,2},3}}
        ,{{{3,3,2},{2,2,3},0},{{1,3,2},{2,2,2},1},{{2,2,3},{3,2,2},2},{{2,3,2},{1,2,2},3}}
        ,{{{3,3,2},{2,2,1},0},{{1,3,2},{2,2,2},1},{{2,2,1},{3,2,2},2},{{2,3,2},{1,2,2},3}}
        ,{{{3,3,2},{2,2,3},0},{{1,3,2},{2,2,2},1},{{2,3,2},{1,2,2},2},{{2,2,3},{3,2,2},3}}
        ,{{{3,3,2},{2,2,1},0},{{1,3,2},{2,2,2},1},{{2,3,2},{1,2,2},2},{{2,2,1},{3,2,2},3}}
};

map<bID,pair<Cell3DPosition,uint8_t>> initConfiguration;
void Catoms3DRotateCode::saveConfiguration() {
    auto modules = Catoms3DWorld::getWorld()->getMap();
    for (auto &module:modules) {
        initConfiguration[module.first]={module.second->position,module.second->orientationCode};
    }
}

void Catoms3DRotateCode::resetConfiguration() {
    auto wrld = Catoms3DWorld::getWorld();
    auto modules = wrld->getMap();
    for (auto &module:modules) {
        lattice->remove(module.second->position, false);
        wrld->disconnectBlock(module.second, false);
    }
    for (auto &module:modules) {
        module.second->setPositionAndOrientation(initConfiguration[module.first].first,initConfiguration[module.first].second);
    }
    for (auto &module:modules) {
        wrld->connectBlock(module.second, false);
    }
}


Catoms3DRotateCode::Catoms3DRotateCode(Catoms3DBlock *host) : Catoms3DBlockCode(host), module(host), myCurrent(order[0]) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;

    // Registers a callback (myAgradientFunc) to the message of type G
    addMessageEventFunc2(UPDATE_MSG_ID,
                         std::bind(&Catoms3DRotateCode::myUpdateFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));
    myDepth = myCurrent.rbegin()->stage;
}

void Catoms3DRotateCode::startup() {
    if (getId()==1) saveConfiguration();;
    lattice = (FCCLattice *) (Catoms3D::getWorld()->lattice);
    myStage = 0;
    auto i = 0;
    while (i < myCurrent.size() && !(myCurrent[i].stage == myStage && myCurrent[i].posFrom == module->position)) {
        i++;
    }
    if (i < myCurrent.size() && myCurrent[i].posFrom == module->position && myCurrent[i].stage == myStage) {
        tryToMove(i);
    }
}

bool Catoms3DRotateCode::tryToMove(int index) {
    vector<std::pair<const Catoms3DMotionRulesLink *, Catoms3DRotation>> tab = Catoms3DMotionEngine::getAllRotationsForModule(module);

    Cell3DPosition finalPos;
    short finalOrient;
    //cout << "possible motions :" << tab.size() << endl;
    for (auto &elem: tab) {
        elem.second.init(((Catoms3DGlBlock *) module->ptrGlBlock)->mat);
        elem.second.getFinalPositionAndOrientation(finalPos, finalOrient);
        cout << "Module #" << module->position << ": " << finalPos << "=?=" << myCurrent[index].posTo << endl;
//        if (finalPos == myCurrent[index].position && finalOrient == myCurrent[index].orient) {
        if (finalPos == myCurrent[index].posTo) {
            scheduler->schedule(
                    new Catoms3DRotationStartEvent(getScheduler()->now() + 100000, module, elem.second.pivot, finalPos));
            cout << "Module #" << getId() << ": " << module->position << " -> " << finalPos << endl;
            return true;
        }
    }
    return false;
}

void Catoms3DRotateCode::onMotionEnd() {
    myStage += 1;
    if (myStage>myDepth) {
        cout << "End of solution #"<< myOrderNum << endl;
        myStage = 0;
        myOrderNum++;
        if (myOrderNum<order.size()) {
            myCurrent = order[myOrderNum];
            // init positions
            resetConfiguration();
            sendMessageToAllNeighbors("updateStage", new MessageOf<pair<int, int>>(UPDATE_MSG_ID, {myStage, myOrderNum}), 15000, 0,0);
        } else {
            setColor(target->getTargetColor(module->position));
            sendMessageToAllNeighbors("updateStage", new MessageOf<pair<int, int>>(UPDATE_MSG_ID, {myStage, myOrderNum}), 15000, 0,0);
            return;
        }
    }
    cout << "oME:" << myStage << "/" << myDepth << endl;
    auto i = 0;
    while (i < myCurrent.size() && !(myCurrent[i].stage == myStage && myCurrent[i].posFrom == module->position)) {
        i++;
    }
    cout << "search stage=" << myStage << "   id= " << getId() << "(" << i << "):";
    if (i < myCurrent.size() && myCurrent[i].stage == myStage && myCurrent[i].posFrom == module->position) {
        cout << "found !"<< endl;
        tryToMove(i);
    } else {
        cout << "not found!" << endl;
    }
    //setColor((target->isInTarget(module->position) ? target->getTargetColor(module->position) : GREY));
    setColor(myStage);
    sendMessageToAllNeighbors("updateStage", new MessageOf<pair<int, int>>(UPDATE_MSG_ID, {myStage, myOrderNum}),
                              15000, 0, 0);
}

void Catoms3DRotateCode::parseUserBlockElements(TiXmlElement *config) {
    const char *attr = config->Attribute("mobile");
    if (attr != nullptr) {
        string str(attr);
        if (str == "true" || str == "1" || str == "yes") {
            isMobile = true;
            std::cout << module->blockId << " is mobile!" << std::endl; // complete with your code
        }
    }
}

void Catoms3DRotateCode::myUpdateFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender) {
    MessageOf<pair<int, int>> *msg = static_cast<MessageOf<pair<int, int>> *>(_msg.get());
    int msgStage = msg->getData()->first;
    int msgmyOrderNum = int(msg->getData()->second);

    console << "rec. myUpdate(" << int(msgStage) << "/" << myStage << "," << msgmyOrderNum << ")\n";
    if (myOrderNum!=msgmyOrderNum && msgmyOrderNum == order.size()) {
        // stop
        myOrderNum = msgmyOrderNum;
        myStage = msgStage;
        setColor(target->getTargetColor(module->position));
        // msgStop
        sendMessageToAllNeighbors("updateStage", new MessageOf<pair<int, int>>(UPDATE_MSG_ID, {myStage, myOrderNum}), 15000, 0, 1, sender);
        return;
    }
    if (msgStage != myStage) {
        myStage = msgStage;
        myOrderNum = msgmyOrderNum;
        myCurrent = order[myOrderNum];
        setColor(myStage);
        cout << "msg search stage=" << myStage << "   id= " << getId() << ":";
        auto i = 0;
        while (i < myCurrent.size() && !(myCurrent[i].stage == myStage && myCurrent[i].posFrom == module->position)) {
            i++;
        }
        if (i < myCurrent.size() && myCurrent[i].stage == myStage && myCurrent[i].posFrom == module->position) {
            cout << "found !" << endl;
            tryToMove(i);
        } else {
            cout << "not found !" << endl;
        }
        sendMessageToAllNeighbors("updateStage", new MessageOf<pair<int, int>>(UPDATE_MSG_ID, {myStage, myOrderNum}), 15000, 0, 0);
    }
}

string Catoms3DRotateCode::onInterfaceDraw() {
    string str="Solution #" + to_string(myOrderNum+1) + " / " + to_string(order.size());
    str+= "\n Step " + to_string(myStage) + " / " + to_string(myDepth);
    return str; // to update with your text
}