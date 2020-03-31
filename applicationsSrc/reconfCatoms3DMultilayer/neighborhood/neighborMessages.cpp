#include "neighborMessages.h"

#define MSG_TIME 0//rand()%100

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

    neighborhood->checkDependencies();
    neighborhood->addNeighbors();

    if (reconf->areNeighborsPlaced() && reconf->nChildren == 0)
        sendMessagePlaneFinished();
}

void NeighborMessages::checkLineParent() {
    if (neighborhood->isFirstCatomOfLine())
        reconf->isLineParent = true;
}

void NeighborMessages::handleNewCatomMsg(MessagePtr message)
{
    shared_ptr<New_catom_message> recv_message = static_pointer_cast<New_catom_message>(message);
    New_catom_response_message *msgResponse = new New_catom_response_message;

    msgResponse->floor = reconf->floor;

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
    nMessagesGetInfo++;

    sendMessagesOnQueue(recv_message->sourceInterface->hostBlock->position);
}

void NeighborMessages::handleNewCatomLineParentMsg(MessagePtr message)
{
    New_catom_line_parent_response_message *msgResponse = new New_catom_line_parent_response_message;
    msgResponse->floor = reconf->floor;

    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msgResponse, message->destinationInterface));
    nMessagesGetInfo++;

    //neighborhood->sendResponseMessageToAddLeft();
    //neighborhood->sendResponseMessageToAddRight();

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
    reconf->interfaceParent = recv_message->sourceInterface;
}

void NeighborMessages::handleNewCatomLineParentResponseMsg(MessagePtr message)
{
    shared_ptr<New_catom_line_parent_response_message> recv_message = static_pointer_cast<New_catom_line_parent_response_message>(message);
    reconf->floor = recv_message->floor;
    reconf->interfaceParent = recv_message->sourceInterface;
}

void NeighborMessages::handleNewCatomPlaneParentResponseMsg(MessagePtr message)
{
    shared_ptr<New_catom_plane_parent_response_message> recv_message = static_pointer_cast<New_catom_plane_parent_response_message>(message);
    reconf->floor = recv_message->floor+1;
}

void NeighborMessages::sendMessageToGetParentInfo()
{
    for (int i = 0; i < 2; i++) {
        Cell3DPosition neighborPosition = (i == 0) ? catom->position.offsetX(-1) : catom->position.offsetX(1);
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

    neighborPosition = catom->position.offsetY(1);
    if (catom->getInterface(neighborPosition)->isConnected()) {
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
        nMessagesGetInfo++;
    }

    neighborPosition = catom->position.offsetY(-1);
    if (catom->getInterface(neighborPosition)->isConnected()) {
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
        nMessagesGetInfo++;
    }
}

void NeighborMessages::sendMessageToGetPlaneParentInfo()
{
    New_catom_plane_parent_message *msg = new New_catom_plane_parent_message;
    Cell3DPosition neighborPosition;

    neighborPosition = catom->position.offsetZ(1);
    if (catom->getInterface(neighborPosition)->isConnected()) {
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(neighborPosition)));
        nMessagesGetInfo++;
    }

    neighborPosition = catom->position.offsetZ(-1);
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

void NeighborMessages::sendMessagePlaneFinished() {
    Plane_finished_message *msg = new Plane_finished_message();
    getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, reconf->interfaceParent->connectedInterface));
}


void NeighborMessages::sendMessagePlaneFinishedAck()
{
    if (reconf->isPlaneCompleted)
        return;
    reconf->isPlaneCompleted = true;

    vector<pair<int,int>> coordinates = {{1,0},{-1,0},{0,-1},{0,1}};
    for (int i = 0; i < 4; i++) {
        int x = coordinates[i].first;
        int y = coordinates[i].second;
        Plane_finished_ack_message *msg = new Plane_finished_ack_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, catom->getInterface(catom->position.offsetX(x).offsetY(y))));
    }
}

void NeighborMessages::sendMessageParentPlaneFinished(Cell3DPosition direction) {
    P2PNetworkInterface *interface = catom->getInterface(direction);
    if (interface->isConnected()) {
        Parent_plane_finished_message *msg = new Parent_plane_finished_message();
        getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + MSG_TIME, msg, interface));
    } else if (BlockCode::target->isInTarget(direction)) {
        Parent_plane_finished_message *msg = new Parent_plane_finished_message();
        MessageQueue message(direction, msg);
        reconf->messageQueue.push_back(message);
    }
}

void NeighborMessages::broadcastMessageParentPlaneFinished()
{
    if (reconf->parentPlaneFinished)
        return;
    reconf->parentPlaneFinished = true;

    vector<pair<int,int>> coordinates = {{1,0},{-1,0},{0,-1},{0,1}};
    for (int i = 0; i < 4; i++) {
        int x = coordinates[i].first;
        int y = coordinates[i].second;
        sendMessageParentPlaneFinished(catom->position.offsetX(x).offsetY(y));
    }
}
