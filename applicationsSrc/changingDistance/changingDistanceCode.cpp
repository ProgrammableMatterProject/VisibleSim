#include "changingDistanceCode.hpp"
#include <limits>

ChangingDistanceCode::ChangingDistanceCode(BlinkyBlocksBlock *host):BlinkyBlocksBlockCode(host),module(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;

    // Registers a callback (myDistanceMessageFunc) to the message of type I
    addMessageEventFunc2(DISTANCEMESSAGE_MSG_ID,
                       std::bind(&ChangingDistanceCode::myDistanceMessageFunc,this,
                       std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myUpdateMessageFunc) to the message of type D
    addMessageEventFunc2(UPDATEMESSAGE_MSG_ID,
                       std::bind(&ChangingDistanceCode::myUpdateMessageFunc,this,
                       std::placeholders::_1, std::placeholders::_2));
    // Registers a callback (myUpdateMessageFunc) to the message of type ?
    addMessageEventFunc2(REMOVEMESSAGE_MSG_ID,
                       std::bind(&ChangingDistanceCode::myRemoveMessageFunc,this,
                       std::placeholders::_1, std::placeholders::_2));

}

void ChangingDistanceCode::startup() {
    console << "start " << module->blockId <<" distance "<<distance<< "\n";
    if (module->blockId == 1) { // Master id is 1
        module->setColor(RED);
        distance = 0;
        console<< " module : "<<module->blockId<<" distance : "<<distance << "\n";
        sendMessageToAllNeighbors("Distance Message", new MessageOf<int>(DISTANCEMESSAGE_MSG_ID, distance+1), 0, 0, 0);
	}
}
void ChangingDistanceCode::myDistanceMessageFunc(std::shared_ptr<Message>_msg, P2PNetworkInterface*sender) {

    MessageOf<int>* msg = static_cast<MessageOf<int>*>(_msg.get());
    int msgData = *msg->getData();
    if (msgData<distance)
    {
        distance = msgData;
        console<< " module : "<<module->blockId<<" distance : "<<distance << "\n";
        module->setColor(Colors[distance % NB_COLORS]);
        sendMessageToAllNeighbors("Distance Message", new MessageOf<int>(DISTANCEMESSAGE_MSG_ID, distance+1), 0, 0, 1,sender);
    }

};

void ChangingDistanceCode::onTap(int face){
    distance = 0 ;
    module->setColor(Colors[distance % NB_COLORS]);
    // console<< " module : "<<module->blockId<<" distance : "<<distance << "\n";
    // console<< " module : "<<module->blockId<<" update : "<<nb_update << "\n";
    nb_update += 1 ;
    sendMessageToAllNeighbors("Update Message", new MessageOf<int>(UPDATEMESSAGE_MSG_ID, nb_update), 0, 0, 0);
    sendMessageToAllNeighbors("Distance Message", new MessageOf<int>(DISTANCEMESSAGE_MSG_ID, distance+1), 0, 0, 0);
};

void ChangingDistanceCode::myUpdateMessageFunc(std::shared_ptr<Message>_msg, P2PNetworkInterface*sender) {
    MessageOf<int>* msg = static_cast<MessageOf<int>*>(_msg.get());
    int msgData = *msg->getData();
    if(nb_update<msgData){
        nb_update = msgData ;
        // console<< " module : "<<module->blockId<<" update : "<<nb_update << "\n";
        distance = MAXINT;
        sendMessageToAllNeighbors("Update Message", new MessageOf<int>(UPDATEMESSAGE_MSG_ID, nb_update), 0, 0, 1,sender);
    }
    
};

void ChangingDistanceCode::myRemoveMessageFunc(std::shared_ptr<Message>_msg, P2PNetworkInterface*sender) {
    MessageOf<int>* msg = static_cast<MessageOf<int>*>(_msg.get());
    int msgData = *msg->getData();
    console <<" Missing neighbor recieved \n";
    if(distance < msgData){
        sendMessage("Distance Message",new MessageOf<int>(DISTANCEMESSAGE_MSG_ID, distance +1),sender,0,0);
    } else {
        sendMessageToAllNeighbors("Missing neighbor Message", new MessageOf<int>(REMOVEMESSAGE_MSG_ID, distance), 0, 0, 0);
        distance = MAXINT ;
    }
}


void ChangingDistanceCode::processLocalEvent(std::shared_ptr<Event> pev) {
    std::shared_ptr<Message> message;
    stringstream info;

    // Do not remove line below
    BlinkyBlocksBlockCode::processLocalEvent(pev);

    switch (pev->eventType) {
        case EVENT_ADD_NEIGHBOR: {
            sendMessageToAllNeighbors("Distance Message", new MessageOf<int>(DISTANCEMESSAGE_MSG_ID, distance+1), 0, 0, 0);
            break;
        }

        case EVENT_REMOVE_NEIGHBOR: {
            sendMessageToAllNeighbors("Missing neighbor Message", new MessageOf<int>(REMOVEMESSAGE_MSG_ID, distance), 0, 0, 0);
            distance = MAXINT;
            break;
        }
    break;
    }
};
