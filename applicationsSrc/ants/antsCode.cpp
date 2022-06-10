#include "antsCode.hpp"
#include <vector>

uint64_t pheromoneMax = 2;

AntsCode::AntsCode(BlinkyBlocksBlock *host) : BlinkyBlocksBlockCode(host), module(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;

    // Registers a callback (myMoveFunc) to the message of type O
    addMessageEventFunc2(MOVE_MSG_ID,
                         std::bind(&AntsCode::myMoveFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));
    isFood = false;
    isNest = false;
    pheromone=0;
}

void AntsCode::startup() {
    console << "start " << getId() << "\n";
    updateColor();
    if (!ants.empty()) {
        auto scheduler=getScheduler();
        scheduler->schedule(new InterruptionEvent<int>(scheduler->now()+getRandomTime(), module, 1));
    }
    for (int i = 0; i < 6; i++) { pheromones[i] = 0; }
}

void AntsCode::updateColor() {
    if (isFood || isNest) return;
    float r = (1.0 * pheromone) / pheromoneMax;
    setColor(Color(0.0f, r, 1.0f - r));
}

void AntsCode::myMoveFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender) {
    MessageOf<pair<uint8_t, uint16_t>> *msg = static_cast<MessageOf<pair<uint8_t, uint16_t>> *>(_msg.get());
    pair<uint8_t, uint16_t> msgData = *msg->getData();
    bool findFood=msgData.first!=0;

    if (isNest && findFood) {
        console << "back to nest" << int(msgData.first) << "\n";
        return;
    }

    //auto senderDir = module->getNbNeighbors() > 1 ? module->getDirection(sender) : 6;
    auto senderDir = module->getDirection(sender);
    ants.push_back(senderDir+msgData.first);
    console << "nAnts=" << ants.size() << "\n";
    if (findFood) pheromone ++;
    if (pheromone > pheromoneMax) {
        pheromoneMax = pheromone;
    }
    pheromones[senderDir] = msgData.second;
    updateColor();
    if (!ants.empty()) {
        auto scheduler=getScheduler();
        scheduler->schedule(new InterruptionEvent<int>(scheduler->now()+getRandomTime(), module, 2));
    }
}

void AntsCode::onInterruptionEvent(shared_ptr<Event> event) {
    auto value = static_cast<int>(static_pointer_cast<InterruptionEvent<int>>(event)->data);
    cout << "Value=" << value << endl;
    if (ants.empty()) return;
    uint8_t antFrom = ants.back() & 0x0f;
    uint8_t antReturns = ants.back() & 0xf0;

    string msg=to_string(antReturns);
    cout << "interrup #" << getId() << " : " << int(antFrom) << " / " << int(antReturns) << endl;

    ants.pop_back();
    if (isFood) {
        sendMessage(new MessageOf<pair<uint8_t, uint32_t>>(MOVE_MSG_ID, pair<uint8_t, uint32_t>(0xf0, pheromone)),
                    module->getInterface(antFrom), 2000, 0);
        pheromones[antFrom]++;
        return;
    }

    // search next direction
    vector<uint8_t> possibleDirs;
    uint32_t sumPheromones = 0;
    for (int i = 0; i < 6; i++) {
        if (module->hasNeighbor(i) && i != antFrom) {
            possibleDirs.push_back(i);
            sumPheromones += pheromones[i]+1;
        }
    }
    if (possibleDirs.empty()) { // locked !
        sendMessage(msg.c_str(),new MessageOf<pair<uint8_t, uint32_t>>(MOVE_MSG_ID, pair<uint8_t, uint32_t>(antReturns, pheromone)),
                    module->getInterface(antFrom), 2000, 0);
        if (antReturns) pheromones[antFrom]++;
    } else if (possibleDirs.size() == 1) { // choose a rand direction
        sendMessage(msg.c_str(),new MessageOf<pair<uint8_t, uint32_t>>(MOVE_MSG_ID, pair<uint8_t, uint32_t>(antReturns, pheromone)),
                    module->getInterface(possibleDirs[0]), 2000, 0);
        if (antReturns) pheromones[possibleDirs[0]]++;
    } else {
        cout << "Possible: " << possibleDirs.size() << "/" << sumPheromones << endl;
        for (auto &p:possibleDirs) {
            cout << int(p) << ":" << int(pheromones[p]) << endl;
        }
        uint32_t p = rand() % sumPheromones;
        cout << "p= " << p << endl;
        int i = 0;
        uint32_t s = pheromones[possibleDirs[i]];
        while (p > s) {
            i++;
            s += pheromones[possibleDirs[i]] + 1;
        }
        //cout << "i=" << i << endl;
        // direction is in possibleDirs[i]
        sendMessage(msg.c_str(),new MessageOf<pair<uint8_t, uint32_t>>(MOVE_MSG_ID, pair<uint8_t, uint32_t>(antReturns, pheromone)),
                    module->getInterface(possibleDirs[i]), 2000, 0);
        if (antReturns) pheromones[possibleDirs[0]]++;
    }
    updateColor();
    if (!ants.empty()) {
        auto scheduler=getScheduler();
        scheduler->schedule(new InterruptionEvent<int>(scheduler->now()+getRandomTime(), module, 4));
    }
}

void AntsCode::parseUserBlockElements(TiXmlElement *config) {
    int Nants = 0;
    const char *attr = config->Attribute("Nants");
    if (attr != nullptr) {
        Nants = atoi(attr);
        if (Nants == 0) Nants = 10;
        std::cout << module->blockId << " has " << Nants << " ants" << std::endl;

        for (int i = 0; i < Nants; i++) {
            ants.push_back(0x08);
        }
    }

    attr = config->Attribute("kind");
    if (attr != nullptr) {
        string str(attr);
        isNest = (str == "nest");
        isFood = (str == "food");
    }
}

void AntsCode::parseUserElements(TiXmlDocument *config) {
    TiXmlNode *node = config->FirstChild("parameters");
    if (!node) return;
    TiXmlElement *element = node->ToElement();
    const char *attr = element->Attribute("Ntotal");
    if (attr != nullptr) {
        //std::cout << "N total = " << Ntotal << " ants" << std::endl;
    }
}

Time AntsCode::getRandomTime() {
    return 5000 + getId();
}