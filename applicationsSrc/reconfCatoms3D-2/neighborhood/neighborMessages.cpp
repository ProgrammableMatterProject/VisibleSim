#include "neighborMessages.h"

#define MSG_TIME rand()%10
#define MSG_TIME_ADD 100+rand()%100
//#define MSG_TIME 0000

int NeighborMessages::nMessagesGetInfo = 0;
int NeighborMessages::nMessagesBorderMessage = 0;

NeighborMessages::NeighborMessages(Catoms3D::Catoms3DBlock *c, Reconf *r, Neighborhood *n)
{
    catom = c;
    reconf = r;
    neighborhood = n;
}

void NeighborMessages::init()
{
    reconf->init = true;
    if (reconf->isPlaneSeed()) {
        catom->setColor(WHITE);
    }

    neighborhood->checkDependencies();
    neighborhood->addNeighbors();
}

void NeighborMessages::checkLineParent() {
    if (neighborhood->isFirstCatomOfLine())
        reconf->setLineParent();
}

//void NeighborMessages::handleParentSeedMsg(MessagePtr message)
//{
    //New_catom_response_message *msgResponse = new New_catom_response_message;
    //getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
//}

void NeighborMessages::handleNewCatomMsg(MessagePtr message)
{
    shared_ptr<New_catom_message> recv_message = static_pointer_cast<New_catom_message>(message);
    New_catom_response_message *msgResponse = new New_catom_response_message;

    msgResponse->floor = reconf->floor;

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
    nMessagesGetInfo++;

    neighborhood->sendResponseMessageToAddLeft();
    neighborhood->sendResponseMessageToAddRight();

    sendMessagesOnQueue(recv_message->sourceInterface->hostBlock->position);
}

void NeighborMessages::handleNewCatomLineParentMsg(MessagePtr message)
{
    New_catom_line_parent_response_message *msgResponse = new New_catom_line_parent_response_message;
    msgResponse->floor = reconf->floor;

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
    nMessagesGetInfo++;

    neighborhood->sendResponseMessageToAddLeft();
    neighborhood->sendResponseMessageToAddRight();

    sendMessagesOnQueue(message->sourceInterface->hostBlock->position);
}

void NeighborMessages::handleNewCatomPlaneParentMsg(MessagePtr message)
{
    New_catom_plane_parent_response_message *msgResponse = new New_catom_plane_parent_response_message;
    msgResponse->floor = reconf->floor;

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
    nMessagesGetInfo++;
}

void NeighborMessages::handleNewCatomResponseMsg(MessagePtr message)
{
    shared_ptr<New_catom_response_message> recv_message = static_pointer_cast<New_catom_response_message>(message);
    reconf->floor = recv_message->floor;
}

void NeighborMessages::handleNewCatomLineParentResponseMsg(MessagePtr message)
{
    shared_ptr<New_catom_line_parent_response_message> recv_message = static_pointer_cast<New_catom_line_parent_response_message>(message);
    reconf->floor = recv_message->floor;
}

void NeighborMessages::handleNewCatomPlaneParentResponseMsg(MessagePtr message)
{
    shared_ptr<New_catom_plane_parent_response_message> recv_message = static_pointer_cast<New_catom_plane_parent_response_message>(message);
    reconf->floor = recv_message->floor+1;
}

void NeighborMessages::sendMessageToGetParentInfo()
{
    for (int i = 0; i < 2; i++) {
        Cell3DPosition neighborPosition = (i == 0) ? catom->position.addX(-1) : catom->position.addX(1);
        New_catom_message *msg = new New_catom_message;
        if (i == 0)
            msg->lineParentDirection = TO_LEFT;
        else
            msg->lineParentDirection = TO_RIGHT;
        if (catom->getInterface(neighborPosition)->isConnected()) {
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
            nMessagesGetInfo++;
        }
    }
}

void NeighborMessages::sendMessageToGetLineParentInfo()
{
    New_catom_line_parent_message *msg = new New_catom_line_parent_message;
    Cell3DPosition neighborPosition;

    neighborPosition = catom->position.addY(1);
    if (catom->getInterface(neighborPosition)->isConnected()) {
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
        nMessagesGetInfo++;
    }

    neighborPosition = catom->position.addY(-1);
    if (catom->getInterface(neighborPosition)->isConnected()) {
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
        nMessagesGetInfo++;
    }
}

void NeighborMessages::sendMessageToGetPlaneParentInfo()
{
    New_catom_plane_parent_message *msg = new New_catom_plane_parent_message;
    Cell3DPosition neighborPosition;

    neighborPosition = catom->position.addZ(1);
    if (catom->getInterface(neighborPosition)->isConnected()) {
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
        nMessagesGetInfo++;
    }

    neighborPosition = catom->position.addZ(-1);
    if (catom->getInterface(neighborPosition)->isConnected()) {
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
        nMessagesGetInfo++;
    }
}

void NeighborMessages::sendMessagesOnQueue(Cell3DPosition pos)
{
    vector<MessageQueue>::iterator it;
    for (it = reconf->messageQueue.begin(); it != reconf->messageQueue.end(); ) {
        if (pos == it->destination) {
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, it->message, catom->getInterface(pos)));
            it = reconf->messageQueue.erase(it);
        }
        else
            ++it;
    }
}
