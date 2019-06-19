/**
 * @file   nodeDemoBlockCode.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:15:26 2019
 *
 * @brief
 *
 *
 */

#include "nodeDemoBlockCode.hpp"

#include "nodeWorld.h"

#include "scheduler.h"
#include "events.h"
#include "trace.h"

using namespace Node;

NodeDemoBlockCode::NodeDemoBlockCode(NodeBlock *host):NodeBlockCode(host) {
    scheduler = getScheduler();
    node = (NodeBlock*)hostBlock;
}

NodeDemoBlockCode::~NodeDemoBlockCode() {
}

// Function called by the module upon initialization
void NodeDemoBlockCode::startup() {
    if (node->hasANeighbor(SLattice::Direction::South)) {
        hasBottomNeighbor = true;
    }

    if (node->hasANeighbor(SLattice::Direction::North)) {
        hasTopNeighbor = true;
    }

    if (not hasBottomNeighbor) {
        setLevel(0);
        updateNeighborLevels();
    }
}

void NodeDemoBlockCode::setLevel(int lvl) {
    level = lvl;
    node->setColor(lvl);
}

bool NodeDemoBlockCode::isTopInterface(const P2PNetworkInterface* itf) const {
    return itf == node->getInterface(SLattice::Direction::North);
}

bool NodeDemoBlockCode::isBottomInterface(const P2PNetworkInterface* itf) const {
    return itf == node->getInterface(SLattice::Direction::South);
}

void NodeDemoBlockCode::handle_levelMessage(std::shared_ptr<MessageOf<int>> msg,
                                           P2PNetworkInterface *sender) {
    int receivedLevel = *msg->getData();

    console << " received Level(" << receivedLevel << ") from "
            << msg->sourceInterface->hostBlock->blockId
            << " at " << getScheduler()->now() << "\n";

    int myLevel; // Deduce my level from level sent by neighbor
    if (isTopInterface(sender)) myLevel = receivedLevel - 1;
    else if (isBottomInterface(sender)) myLevel = receivedLevel + 1;
    else myLevel = receivedLevel;

    if (level == -1 or myLevel > level) {
        setLevel(myLevel);
        updateNeighborLevels();
    }
}

void NodeDemoBlockCode::updateNeighborLevels() {
    stringstream info;
    info << "Level(" << level << ")";
    sendMessageToAllNeighbors(info.str().c_str(),
                              new MessageOf<int>(LEVEL_MSG, level), 100, 0, 0);
}

void NodeDemoBlockCode::processReceivedMessage(MessagePtr msg,
                                              P2PNetworkInterface *sender) {
    stringstream info;

    switch (msg->type) {
        case LEVEL_MSG: {
            std::shared_ptr<MessageOf<int>> levelMsg =
                std::static_pointer_cast<MessageOf<int>>(msg);

            handle_levelMessage(levelMsg, sender);
        } break;
        default:
            cout << "Unknown Generic Message Type" << endl;
            assert(false);
            break;
    }

}

void NodeDemoBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;

    switch (pev->eventType) {
        case EVENT_RECEIVE_MESSAGE: {
            message =
                (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;

            if (message->isMessageHandleable()) {
                (std::static_pointer_cast<HandleableMessage>(message))->handle(this);
            } else {
                P2PNetworkInterface * recv_interface = message->destinationInterface;

                // Handover to global message handler
                processReceivedMessage(message, recv_interface);
            }
        } break;

        case EVENT_ROTATION3D_END: {

        } break;

        case EVENT_TAP: {
        } break;
    }
}
