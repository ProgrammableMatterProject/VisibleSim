/*
 * blockCode.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom, bpiranda
 */

#include <iostream>
#include "blockCode.h"
#include "world.h"

using namespace std;

namespace BaseSimulator {

    Target *BlockCode::target = nullptr;

    BlockCode::InterfaceNotConnectedException::
    InterfaceNotConnectedException(BlockCode *bc, const Message *msg,
                                   const P2PNetworkInterface *itf) {
        stringstream ss;
        uint8_t itfId = bc->hostBlock->getInterfaceBId(itf);
        Cell3DPosition nPos;
        bool err = not bc->hostBlock->getNeighborPos(itfId, nPos);
        ss << "Trying to send " << msg->getMessageName() << " through unconnected interface: "
           << " { sender = #" << bc->hostBlock->blockId
           << " at " << bc->hostBlock->position
           << ", itfId = " << (int) itfId
           << ", nPos = " << (string) (err ? "#ERROR" : bc->hostBlock->position.to_string())
           << " }" << endl;
        m_msg = ss.str();
    }

    BlockCode::BlockCode(BuildingBlock *host) : hostBlock(host) {
        if (host) {
            scheduler = getScheduler();
            lattice = getWorld()->lattice;
            console.setInfo(scheduler, hostBlock->blockId);
            addDebugAttributes(scheduler);
        }
    }

    BlockCode::~BlockCode() {
        if (target) {
            delete target;
            target = nullptr;
        }
        eventFuncMap2.clear();
    }

    void BlockCode::addMessageEventFunc2(int type, eventFunc2 func) {
        eventFuncMap2.insert(pair<int, eventFunc2>(type, func));
    }

    int BlockCode::sendMessage(Message *msg, P2PNetworkInterface *dest, Time t0, Time dt) {
        return sendMessage(nullptr, msg, dest, t0, dt);
    }

    int BlockCode::sendMessage(HandleableMessage *msg,
                               P2PNetworkInterface *dest, Time t0, Time dt) {
        // PTHY: t1: Risque que deux messages envoyés sequentiellement au même t0 ne soient pas envoyés dans l'ordre ???
        Time t1 = scheduler->now() + t0;
        // + (Time)(((double)dt*hostBlock->getRandomUint())/((double)uintRNG::max()));

        if (not dest->connectedInterface) {
            throw InterfaceNotConnectedException(this, msg, dest);
        }

        console << " sends " << msg->getName() << " to "
                << dest->getConnectedBlockId() << " at " << t1 << "\n";
#ifdef DEBUG_MESSAGES
        OUTPUT << "#" << hostBlock->blockId << " " << hostBlock->position
           << " sends " << msg->type << " to "
           << dest->connectedInterface->hostBlock->blockId << " at " << t1 << endl;
#endif

        VS_ASSERT(dest->getConnectedBlockId() > 0);

        scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(t1, msg, dest));
        return 0;
    }

    int BlockCode::sendMessage(const char *msgString, Message *msg,
                               P2PNetworkInterface *dest, Time t0, Time dt) {
        // PTHY: t1: Risque que deux messages envoyés sequentiellement au même t0 ne soient pas envoyés dans l'ordre ???
        Time t1 = scheduler->now() + t0;
        // + (Time)(((double)dt*hostBlock->getRandomUint())/((double)uintRNG::max()));

        if (not dest->connectedInterface) {
            throw InterfaceNotConnectedException(this, msg, dest);
        }
        if (msgString)
            console << " sends " << msgString << " to "
                    << dest->getConnectedBlockId() << " at " << t1 << "\n";
        else if (msg->isMessageHandleable())
            console << " sends " << msg->getMessageName() << " to "
                    << dest->getConnectedBlockId() << " at " << t1 << "\n";

#ifdef DEBUG_MESSAGES
        OUTPUT << hostBlock->blockId << " sends " << msg->type << " to "
           << dest->connectedInterface->hostBlock->blockId << " at " << t1 << endl;
#endif

        scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(t1, msg, dest));
        return 0;
    }

    int BlockCode::sendMessageToAllNeighbors(Message *msg, Time t0, Time dt, int nexcept, ...) {
        va_list args;
        va_start(args, nexcept);
        int ret = sendMessageToAllNeighbors(nullptr, msg, t0, dt, nexcept, args);
        va_end(args);

        return ret;
    }

    int BlockCode::sendMessageToAllNeighbors(const char *msgString, Message *msg,
                                             Time t0, Time dt, int nexcept, ...) {
        va_list args;
        va_start(args, nexcept);
        int ret = sendMessageToAllNeighbors(msgString, msg, t0, dt, nexcept, args);
        va_end(args);

        return ret;
    }

    map<short, P2PNetworkInterface *> BlockCode::getAllConnectedInterfaces() {
        map<short, P2PNetworkInterface *> res;
        P2PNetworkInterface *p2p;
        for (short i = 0; i < hostBlock->getNbInterfaces(); i++) {
            p2p = hostBlock->getInterface(i);
            if (p2p->connectedInterface) {
                res[i] = p2p;
            }
        }
        return res;
    }

    int BlockCode::sendMessageToAllNeighbors(const char *msgString, Message *msg,
                                             Time t0, Time dt, int nexcept, va_list args) {
        P2PNetworkInterface *tabExceptions[hostBlock->getNbInterfaces()];
        for (int i = 0; i < nexcept; i++) {
            tabExceptions[i] = va_arg(args, P2PNetworkInterface*);
        }

        P2PNetworkInterface *p2p;
        int j, n = 0;
        for (int i = 0; i < hostBlock->getNbInterfaces(); i++) {
            p2p = hostBlock->getInterface(i);
            if (p2p->connectedInterface) { // on regarde si elle n'est pas dans les interdits
                j = 0;
                while (j < nexcept && p2p != tabExceptions[j]) j++;
                if (j == nexcept) {
                    sendMessage(msgString, msg->clone(), p2p, t0, dt);
                    Message::incrementMessageCounts();
                    n++;
                }
            }
        }

        Message::adjustClonedMessageCount();
        delete msg;

        return n;
    }

    void BlockCode::processLocalEvent(EventPtr pev) {
        MessagePtr message;
        stringstream info;

        // cout << "event #" << pev->id << ":" << pev->eventType << endl;
        switch (pev->eventType) {
            case EVENT_NI_RECEIVE: {
                message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
                // search message id in eventFuncMap
                multimap<int, eventFunc2>::iterator im2 = eventFuncMap2.find(message->type);
                if (im2 != eventFuncMap2.end()) {
                    P2PNetworkInterface *recv_interface = message->destinationInterface;
                    (*im2).second(message, recv_interface);
                } else {
                    OUTPUT << "ERROR: message Id #" << message->type << " unknown!" << endl;
                }
            }
                break;
            case EVENT_REMOVE_NEIGHBOR: {
                uint64_t face = (std::static_pointer_cast<RemoveNeighborEvent>(pev))->face;
                onNeighborChanged(face, pev->eventType);
            }
                break;
            case EVENT_ADD_NEIGHBOR: {
                uint64_t face = (std::static_pointer_cast<RemoveNeighborEvent>(pev))->face;
                onNeighborChanged(face, pev->eventType);
            }
                break;
            case EVENT_TAP: {
                int face = (std::static_pointer_cast<TapEvent>(pev))->tappedFace;
                onTap(face);
            }
                break;
            case EVENT_TELEPORTATION_END: {
                onMotionEnd();
            }
                break;
            case EVENT_INTERRUPTION: {
                onInterruptionEvent(pev);
            }
                break;
        }
    }

    void BlockCode::onTap(int face) {
        stringstream info;
        info.str("");
        info << "Tapped on face " << lattice->getDirectionString(face);
        scheduler->trace(info.str(), hostBlock->blockId);
    }

    bool BlockCode::loadNextTarget() {
        target = Target::loadNextTarget();

        return target != nullptr;
    }

    string BlockCode::onInterfaceDraw() {
        return "#Modules: " + to_string(BaseSimulator::getWorld()->lattice->nbModules);
    }

    bID BlockCode::getId() const {
        return hostBlock ? hostBlock->blockId : 0;
    }

    void BlockCode::setColor(const Color &color) {
        if (hostBlock) hostBlock->setColor(color);
    }

    void BlockCode::setColor(int idColor) {
        if (hostBlock) hostBlock->setColor(idColor);
    }

    void BlockCode::setVisible(bool v) {
        if (hostBlock) hostBlock->getGlBlock()->setVisible(v);
    }

    Color BlockCode::getColor() const {
        return (hostBlock ? hostBlock->color : BLACK);
    }
} // BaseSimulator namespace
