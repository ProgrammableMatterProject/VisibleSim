#include "antsCode.hpp"
#include <vector>
#include <random>
#include <cmath>

uint64_t pheromoneMax = 100;
random_device rd;
default_random_engine re(rd());
int param_a=10;

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
    if (isFood) module->setBlinkMode(true);
}

void AntsCode::updateColor() {
    if (isFood || isNest) return;
    float r = (1.0 * pheromone) / pheromoneMax;
    setColor(Color(0.0f, r, 1.0f - r));
}

void AntsCode::myMoveFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender) {
    MessageOf<pair<AntsData,uint16_t>> *msg = static_cast<MessageOf<pair<AntsData,uint16_t>> *>(_msg.get());
    pair<AntsData,uint16_t> msgData = *msg->getData();
    AntsData antData = msgData.first;

    if (isNest && msg) {
        console << "back to nest" << int(antData.id) << "\n";
        return;
    }

    antData.from = module->getDirection(sender);
    if (antData.state==ANTS_BACK && antData.pheromone_reserve>0) {
        pheromone++;
        if (pheromone > pheromoneMax) {
            pheromoneMax = pheromone;
        }
        antData.pheromone_reserve--;
    }

    ants.push_back(antData);
    console << "nAnts=" << ants.size() << "\n";

    pheromones[antData.from] = msgData.second;
    updateColor();
    if (!ants.empty()) {
        auto scheduler=getScheduler();
        scheduler->schedule(new InterruptionEvent<int>(scheduler->now()+getRandomTime(), module, 2));
    }
}

void AntsCode::onInterruptionEvent(shared_ptr<Event> event) {
    //auto value = static_cast<int>(static_pointer_cast<InterruptionEvent<int>>(event)->data);
    //cout << "Value=" << value << endl;
    if (ants.empty()) return;
    setColor(ants.size());
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
            sumPheromones += param_a+pheromones[i];
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
            cout << int(p) << "(" << module->getInterface(p)->getConnectedBlockId() << "):" << int(pheromones[p]) << endl;
        }
        uniform_int_distribution<uint32_t>uniform_dist(0,sumPheromones-1);
        uint32_t p=uniform_dist(re);
        cout << "p= " << p << endl;
        int i = 0;
        uint32_t s = param_a+pheromones[possibleDirs[i]];
        while (p > s) {
            i++;
            s += param_a+pheromones[possibleDirs[i]];
        }
        cout << "interface=" << i << " -> " << module->getInterface(possibleDirs[i])->getConnectedBlockId() << endl;
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