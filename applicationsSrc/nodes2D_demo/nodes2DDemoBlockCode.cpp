/**
 * @file   nodes2DDemoBlockCode.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:15:26 2019
 *
 * @brief
 *
 *
 */

#include "nodes2DDemoBlockCode.hpp"

#include "robots/nodes2D/nodes2DWorld.h"

#include "events/scheduler.h"
#include "events/events.h"
#include "utils/trace.h"
#include "robots/nodes2D/nodes2DMotionEvents.h"
#include "robots/nodes2D/nodes2DMotionEngine.h"

using namespace Nodes2D;

Nodes2DDemoBlockCode::Nodes2DDemoBlockCode(Nodes2DBlock *host):Nodes2DBlockCode(host) {
    scheduler = getScheduler();
    nodes2D = (Nodes2DBlock*)hostBlock;
}

Nodes2DDemoBlockCode::~Nodes2DDemoBlockCode() {
}

// Function called by the module upon initialization
void Nodes2DDemoBlockCode::startup() {
    Nodes2DWorld *wrl = Nodes2D::getWorld();
    // Dummy translation example
    if (nodes2D->blockId == 36) {
        // turn clockwise if possible !
        vector<Nodes2DMotion*> tab = wrl->getAllMotionsForModule(nodes2D);
        vector<Nodes2DMotion*>::const_iterator ci=tab.begin();
        while (ci!=tab.end() && (*ci)->direction!=motionDirection::CW) {
                ci++;
        }
        if (ci!=tab.end()) {
            Cell3DPosition destination = nodes2D->position + (*ci)->finalPos;
            previousPivot = (*ci)->toConId;
            cout << "previousPivot=" << (*ci)->toConId << "   md=" << (*ci)->direction << endl;
            scheduler->schedule(new Nodes2DMotionStartEvent(scheduler->now()+1000000, nodes2D,destination,(*ci)->toConId));
        }

    } else {
        updateRainbowState();
    }
}

void Nodes2DDemoBlockCode::updateRainbowState() {
    if (nodes2D->hasANeighbor(SLattice::Direction::South)) {
        hasBottomNeighbor = true;
    }

    if (nodes2D->hasANeighbor(SLattice::Direction::North)) {
        hasTopNeighbor = true;
    }

    if (not hasBottomNeighbor) {
        setLevel(0);
        updateNeighborLevels();
    }
}

void Nodes2DDemoBlockCode::setLevel(int lvl) {
    level = lvl;
    nodes2D->setColor(lvl);
}

bool Nodes2DDemoBlockCode::isTopInterface(const P2PNetworkInterface* itf) const {
    return itf == nodes2D->getInterface(SLattice::Direction::North);
}

bool Nodes2DDemoBlockCode::isBottomInterface(const P2PNetworkInterface* itf) const {
    return itf == nodes2D->getInterface(SLattice::Direction::South);
}

void Nodes2DDemoBlockCode::handle_levelMessage(std::shared_ptr<MessageOf<int>> msg,
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

void Nodes2DDemoBlockCode::updateNeighborLevels() {
    stringstream info;
    info << "Level(" << level << ")";
    sendMessageToAllNeighbors(info.str().c_str(),
                              new MessageOf<int>(LEVEL_MSG, level), 100, 0, 0);
}

void Nodes2DDemoBlockCode::processReceivedMessage(MessagePtr msg,
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

void Nodes2DDemoBlockCode::processLocalEvent(EventPtr pev) {
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

        case EVENT_NODES2DMOTION_END: {
            motionEnd();
        } break;

        case EVENT_TAP: {
        } break;
    }
}

void Nodes2DDemoBlockCode::motionEnd() {
    // turn clockwise from previousPivot attachment
    Nodes2DWorld *wrl = Nodes2D::getWorld();
    vector<Nodes2DMotion*> tab = wrl->getAllMotionsForModule(nodes2D);
    vector<Nodes2DMotion*>::const_iterator ci=tab.begin();
    while (ci!=tab.end() && !((*ci)->direction==motionDirection::CW && (*ci)->fromConId==previousPivot)) {
        ci++;
    }
    if (ci!=tab.end()) {
        Cell3DPosition destination = nodes2D->position + (*ci)->finalPos;
        previousPivot = (*ci)->toConId;
        cout << "previousPivot=" << (*ci)->toConId << "   md=" << (*ci)->direction << endl;
        scheduler->schedule(new Nodes2DMotionStartEvent(scheduler->now()+1000000, nodes2D,destination,(*ci)->toConId));
    }
}
