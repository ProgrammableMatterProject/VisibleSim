/**
 * @file   c2dDemoBlockCode.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 10:10:25 2019
 *
 * @brief
 *
 *
 */

#include "c2dDemoBlockCode.hpp"

#include "robots/catoms2D/catoms2DWorld.h"

#include "events/scheduler.h"
#include "events/events.h"
#include "utils/trace.h"

using namespace Catoms2D;

C2dDemoBlockCode::C2dDemoBlockCode(Catoms2DBlock *host):Catoms2DBlockCode(host) {
    scheduler = getScheduler();
    catom = (Catoms2DBlock*)hostBlock;
}

C2dDemoBlockCode::~C2dDemoBlockCode() {
}

// Function called by the module upon initialization
void C2dDemoBlockCode::startup() {
    if (catom->hasANeighbor(HLattice::Direction::BottomRight)
        or catom->hasANeighbor(HLattice::Direction::BottomLeft)) {
        hasBottomNeighbor = true;
    }

    if (catom->hasANeighbor(HLattice::Direction::TopRight)
        or catom->hasANeighbor(HLattice::Direction::TopLeft)) {
        hasTopNeighbor = true;
    }

    if (not hasBottomNeighbor) {
        setLevel(0);
        updateNeighborLevels();
    }
}

void C2dDemoBlockCode::setLevel(int lvl) {
    level = lvl;
    catom->setColor(lvl);
}

bool C2dDemoBlockCode::isTopInterface(const P2PNetworkInterface* itf) const {
    return itf == catom->getInterface(HLattice::Direction::TopRight)
        or itf == catom->getInterface(HLattice::Direction::TopLeft);
}

bool C2dDemoBlockCode::isBottomInterface(const P2PNetworkInterface* itf) const {
    return itf == catom->getInterface(HLattice::Direction::BottomRight)
        or itf == catom->getInterface(HLattice::Direction::BottomLeft);
}

void C2dDemoBlockCode::handle_levelMessage(std::shared_ptr<MessageOf<int>> msg,
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

void C2dDemoBlockCode::updateNeighborLevels() {
    stringstream info;
    info << "Level(" << level << ")";
    sendMessageToAllNeighbors(info.str().c_str(),
                              new MessageOf<int>(LEVEL_MSG, level), 100, 0, 0);
}

void C2dDemoBlockCode::processReceivedMessage(MessagePtr msg,
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

void C2dDemoBlockCode::processLocalEvent(EventPtr pev) {
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
