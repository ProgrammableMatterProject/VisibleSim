#include "antsCode.hpp"

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
    if (module->blockId == 1) { // Master id is 1
        module->setColor(RED);
    }
}
void AntsCode::myMoveFunc(std::shared_ptr<Message>_msg, P2PNetworkInterface*sender) {

    MessageOf<pair<uint8_t,uint16_t>>* msg = static_cast<MessageOf<pair<uint8_t,uint16_t>>*>(_msg.get());
    pair<uint8_t,uint16_t> msgData = *msg->getData();
}

void AntsCode::parseUserBlockElements(TiXmlElement *config) {
    const char *attr = config->Attribute("Nants");
    if (attr!=nullptr) {
        Nants = atoi(attr);
        if (Nants==0) Nants=100;
        std::cout << module->blockId << " has " << Nants << " ants" << std::endl;
    }
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