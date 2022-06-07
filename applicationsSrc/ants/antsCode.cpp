#include "antsCode.hpp"
#include <vector>

uint64_t Ntotal=100;

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
    if (Nants>0) {
        for (int i=0; i<6; i++) {
            if (module->hasNeighbor(i)) {
                Nants--;
                tabCoef[i]++;
                sendMessage(new MessageOf<pair<uint8_t,uint16_t>>(MOVE_MSG_ID,pair<uint8_t,uint16_t>(1,pheromone)),
                            module->getInterface(i),1000,0);
            }
        }
        getScheduler()->schedule(new InterruptionEvent(getScheduler()->now()+5000+getId(),module, 6));
    }
}

void AntsCode::setColor() {
    float c=1.0*pheromone/Ntotal;
    if (c<1.0) {
        module->setColor(Color(0.0,c,1.0-c));
    } else {
        module->setColor(Color(c-1.0,1.0,0));
    }
}

void AntsCode::myMoveFunc(std::shared_ptr<Message>_msg, P2PNetworkInterface*sender) {

    MessageOf<pair<uint8_t,uint16_t>>* msg = static_cast<MessageOf<pair<uint8_t,uint16_t>>*>(_msg.get());
    pair<uint8_t,uint16_t> msgData = *msg->getData();

    Nants+=msgData.first;
    int senderDir = module->getDirection(sender);
    tabCoef[senderDir]=msgData.second;
    pheromone+=msgData.first;
    setColor();

    if (Nants!=0) {
        // Schedule next motion of Ants
        getScheduler()->schedule(new InterruptionEvent(getScheduler()->now()+5000+getId(),module, senderDir));
    }
}

void AntsCode::onInterruption(uint64_t fromDir) {
    if (!Nants) return;
    vector<uint8_t> tabPossibilities;
    uint32_t sum=0;
    for (int i=0; i<6; i++) {
        if (module->hasNeighbor(i) && i!=fromDir) {
            tabPossibilities.push_back(i);
            sum+=tabPossibilities[i];
        }
    }
    if (tabPossibilities.size()==0) return;
    if (tabPossibilities.size()==1) {
        Nants--;

    }
    if (sum==0) {

    }

    if (Nants!=0) {
        getScheduler()->schedule(new InterruptionEvent(getScheduler()->now()+5000+getId(),module, IT_WAKEUP_ID));
    }
}

void AntsCode::parseUserBlockElements(TiXmlElement *config) {
    Nants=0;
    const char *attr = config->Attribute("Nants");
    if (attr!=nullptr) {
        Nants = atoi(attr);
        if (Nants == 0) Nants = 100;
        std::cout << module->blockId << " has " << Nants << " ants" << std::endl;
    }
    pheromone=Nants;
}

void AntsCode::parseUserElements(TiXmlDocument *config) {
    TiXmlNode *node = config->FirstChild("parameters");
    if (!node) return;
    TiXmlElement* element = node->ToElement();
    const char *attr = element->Attribute("Ntotal");
    if (attr!=nullptr) {
        Ntotal = atoi(attr);
        std::cout << "N total = " << Ntotal << " ants" << std::endl;
    }

}