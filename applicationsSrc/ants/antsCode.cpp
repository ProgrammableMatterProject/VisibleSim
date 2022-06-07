#include "antsCode.hpp"

uint64_t pheromoneMax=100;

AntsCode::AntsCode(BlinkyBlocksBlock *host):BlinkyBlocksBlockCode(host),module(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;

    // Registers a callback (myMoveFunc) to the message of type O
    addMessageEventFunc2(MOVE_MSG_ID,
                         std::bind(&AntsCode::myMoveFunc,this,
                                   std::placeholders::_1, std::placeholders::_2));

}

void AntsCode::startup() {
    console << "start " << module->blockId << "\n";
    setColor();
    if (!ants.empty()) {
        getScheduler()->schedule(new InterruptionEvent(getRandomTime(), module, 0));
    }
    for  (int i=0; i<6; i++) { pheromones[i]=0; }
}

void AntsCode::setColor() {
    float r = (1.0*pheromone)/pheromoneMax;
    module->setColor(Color(0.0f,r,1.0f-r));
}

void AntsCode::myMoveFunc(std::shared_ptr<Message>_msg, P2PNetworkInterface*sender) {

    MessageOf<pair<uint8_t,uint16_t>>* msg = static_cast<MessageOf<pair<uint8_t,uint16_t>>*>(_msg.get());
    pair<uint8_t,uint16_t> msgData = *msg->getData();

    auto senderDir=module->getNbNeighbors()>1?module->getDirection(sender):6;
    //auto senderDir=module->getDirection(sender);
    for (int i=0; i<msgData.first; i++) {
        ants.push_back(senderDir);
    }
    console << "nAnts=" << ants.size() << "\n";
    pheromone+=msgData.first;
    if (pheromone>pheromoneMax) {
        pheromoneMax=pheromone;
    }
    pheromones[senderDir]=msgData.second;
    setColor();
    if (!ants.empty()) {
        getScheduler()->schedule(new InterruptionEvent(getRandomTime(), module, 0));
    }
}

void AntsCode::onInterruptionEvent(uint64_t data) {
    if (ants.empty()) return;
    uint8_t antFrom = ants.back();
    cout << "interrup #" << getId() << " : " << int(antFrom) << " / " << ants.size() << endl;
    vector<uint8_t> possibleDirs;
    uint32_t sumPheromones=0;
    for (int i=0; i<6; i++) {
        if (module->hasNeighbor(i) && i!=antFrom) {
            possibleDirs.push_back(i);
            sumPheromones+=pheromones[i]+1;
        }
    }
    if (possibleDirs.empty()) return; // locked !
    ants.pop_back();
    // choose a rand direction
    if (possibleDirs.size()==1) {
        sendMessage(new MessageOf<pair<uint8_t,uint32_t>>(MOVE_MSG_ID,pair<uint8_t,uint32_t>(1,pheromone)),
                    module->getInterface(possibleDirs[0]),2000,0);
        pheromones[possibleDirs[0]]++;
    } else {
        /*cout << "Possible: " << possibleDirs.size() << "/" << sumPheromones << endl;
        for (auto &p:possibleDirs) {
            cout << int(p) << ":" << int(pheromones[p]) << endl;
        }*/
        uint32_t p = rand()%sumPheromones;
        //cout << "p= " << p << endl;
        int i=0;
        uint32_t s=pheromones[possibleDirs[i]];
        while (p>s) {
            i++;
            s+=pheromones[possibleDirs[i]]+1;
        }
        //cout << "i=" << i << endl;
        // direction is in possibleDirs[i]
        sendMessage(new MessageOf<pair<uint8_t,uint32_t>>(MOVE_MSG_ID,pair<uint8_t,uint32_t>(1,pheromone)),
                    module->getInterface(possibleDirs[i]),2000,0);
        pheromones[possibleDirs[i]]++;
    }
    setColor();
    if (!ants.empty()) {
        getScheduler()->schedule(new InterruptionEvent(getRandomTime(), module, 0));
    }
}

void AntsCode::parseUserBlockElements(TiXmlElement *config) {
    int Nants=0;
    const char *attr = config->Attribute("Nants");
    if (attr!=nullptr) {
        Nants = atoi(attr);
        if (Nants==0) Nants=100;
        std::cout << module->blockId << " has " << Nants << " ants" << std::endl;
    }
    for (int i=0; i<Nants; i++) {
        ants.push_back(6);
    }
    pheromone=Nants;
    if (pheromone>pheromoneMax) pheromoneMax=pheromone;
}

void AntsCode::parseUserElements(TiXmlDocument *config) {
    TiXmlNode *node = config->FirstChild("parameters");
    if (!node) return;
    TiXmlElement* element = node->ToElement();
    const char *attr = element->Attribute("Ntotal");
    if (attr!=nullptr) {
        //std::cout << "N total = " << Ntotal << " ants" << std::endl;
    }
}

Time AntsCode::getRandomTime() {
    return getScheduler()->now()+5000+getId();
}