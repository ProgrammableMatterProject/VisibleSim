#include "robotBlocksGeneric.h"
#include <stdarg.h>

namespace RobotBlocks {

void GenericCodeBlock::addMessageEventFunc(int type,eventFunc func) {
    eventFuncMap.insert(pair<int,eventFunc>(type,func));
}

int GenericCodeBlock::sendMessage(Message*msg,P2PNetworkInterface *dest,int t0,int dt) {
    int t1 = scheduler->now() + t0 + (int)(((double)dt*rand())/RAND_MAX);

    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(t1, msg, dest));
    return 0;
}

int GenericCodeBlock::sendMessage(const char*msgString,Message*msg,P2PNetworkInterface *dest,int t0,int dt) {
    int t1 = scheduler->now() + t0 + (int)(((double)dt*rand())/RAND_MAX);

    console << " sends " << msgString << " to " << dest->getConnectedBlockId() << " at " << t1 << "\n";
    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(t1, msg, dest));
    return 0;
}


int GenericCodeBlock::sendMessageToAllNeighbors(Message*msg,int t0,int dt,int nexcept,...) {
    va_list ap;
    P2PNetworkInterface *tabExceptions[6];
//    int n = va_arg(vl,int);
    va_start(ap,nexcept);
    for (int i=0; i<nexcept; i++) {
        tabExceptions[i] = va_arg(ap,P2PNetworkInterface*);
    }
    va_end(ap);

    P2PNetworkInterface *p2p;
    int j,n=0,t1;
    for (int i=0; i<6; i++) {
        p2p = module->getInterface((NeighborDirection::Direction)(i));
        if(p2p && p2p->connectedInterface) { // on regarde si elle n'est pas dans les interdits
            j=0;
            while (j<nexcept && p2p!=tabExceptions[j]) j++;
            if (j==nexcept) {
                t1 = scheduler->now() + t0 + (int)(((double)dt*rand())/RAND_MAX);
                OUTPUT << module->blockId << " sends " << msg->type << " to " << p2p->connectedInterface->hostBlock->blockId << " at " << t1 << endl;
                Message* msg_clone = msg->clone();
                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(t1, msg_clone, p2p));
                n++;
            }
        }
    }
    delete msg;
    return n;
}

int GenericCodeBlock::sendMessageToAllNeighbors(const char*msgString, Message*msg,int t0,int dt,int nexcept,...) {
    va_list ap;
    P2PNetworkInterface *tabExceptions[12];
//    int n = va_arg(vl,int);
    va_start(ap,nexcept);
    for (int i=0; i<nexcept; i++) {
        tabExceptions[i] = va_arg(ap,P2PNetworkInterface*);
    }
    va_end(ap);

    P2PNetworkInterface *p2p;
    int j,n=0,t1;
    for (int i=0; i<6; i++) {
        p2p = module->getInterface((NeighborDirection::Direction)(i));
        if(p2p->connectedInterface) { // on regarde si elle n'est pas dans les interdits
            j=0;
            while (j<nexcept && p2p!=tabExceptions[j]) j++;
            if (j==nexcept) {
                t1 = scheduler->now() + t0 + (int)(((double)dt*rand())/RAND_MAX);
                console << " sends " << msgString << " to " << p2p->getConnectedBlockId() << " at " << t1 << "\n";
                OUTPUT << module->blockId << " sends " << msg->type << " to " << p2p->connectedInterface->hostBlock->blockId << " at " << t1 << endl;
                Message* msg_clone = msg->clone();
                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(t1, msg_clone, p2p));
                n++;
            }
        }
    }
    delete msg;
    return n;
}



void GenericCodeBlock::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;

    if (pev->eventType == EVENT_NI_RECEIVE) {
        message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
// search message id in eventFuncMap
        multimap<int,eventFunc>::iterator im = eventFuncMap.find(message->type);
        if (im!=eventFuncMap.end()) {
            P2PNetworkInterface *recv_interface = message->destinationInterface;
            (*im).second(this,message,recv_interface);
        } else {
            OUTPUT << "ERROR: message Id #"<< message->type << " unknown!" << endl;
        }

    }
}

}
