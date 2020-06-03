#include "tetrisCode.hpp"

TetrisCode::TetrisCode(BlinkyBlocksBlock *host):BlinkyBlocksBlockCode(host),module(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;

    // Registers a callback (myHeightMsgFunc) to the message of type E
    addMessageEventFunc2(HEIGHTMSG_MSG_ID,
                       std::bind(&TetrisCode::myHeightMsgFunc,this,
                       std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myWidthMsgFunc) to the message of type D
    addMessageEventFunc2(WIDTHMSG_MSG_ID,
                       std::bind(&TetrisCode::myWidthMsgFunc,this,
                       std::placeholders::_1, std::placeholders::_2));

}

void TetrisCode::startup() {
    console << "start " << module->blockId << "\n";
    if (module->blockId == 1) { // Master id is 1
        module->setColor(RED);
	}
}
void TetrisCode::myHeightMsgFunc(std::shared_ptr<Message>_msg, P2PNetworkInterface*sender) {

    MessageOf<int>* msg = static_cast<MessageOf<int>*>(_msg.get());
    int msgData = *msg->getData();

};

void TetrisCode::myWidthMsgFunc(std::shared_ptr<Message>_msg, P2PNetworkInterface*sender) {

    MessageOf<int>* msg = static_cast<MessageOf<int>*>(_msg.get());
    int msgData = *msg->getData();

};


void TetrisCode::processLocalEvent(std::shared_ptr<Event> pev) {
    std::shared_ptr<Message> message;
    stringstream info;

    // Do not remove line below
    BlinkyBlocksBlockCode::processLocalEvent(pev);

    switch (pev->eventType) {
        case EVENT_ADD_NEIGHBOR: {
            // Do something when a neighbor is added to an interface of the module
            break;
        }

        case EVENT_REMOVE_NEIGHBOR: {
            // Do something when a neighbor is removed from an interface of the module
            break;
        }

        case EVENT_INTERRUPTION: {
            // Do something when the module receives an event
            break;
        }
    break;
    }
};

void TetrisCode::onUserKeyPressed(unsigned char c, int x, int y) {
    switch (c) {
        case 'a' : // update with your code
        break;
    }
};
