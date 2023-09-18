/**
 * @file   okteenBlockCode.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:55:11 2019
 *
 * @brief
 *
 *
 */

#include <iostream>
#include "../../comm/network.h"
#include "../../utils/trace.h"
#include "hexanodesBlockCode.h"
#include "hexanodesMotionEvents.h"
#include "hexanodesMotionEngine.h"

using namespace std;
using namespace Hexanodes;

HexanodesBlockCode::HexanodesBlockCode(HexanodesBlock *host):BlockCode(host) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "HexanodesBlockCode constructor" << endl;
#endif
}

HexanodesBlockCode::~HexanodesBlockCode() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "HexanodesBlockCode destructor" << endl;
#endif
}

void HexanodesBlockCode::addDebugAttributes(Scheduler *scheduler) {
}

void HexanodesBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;

//cout << "event #" << pev->id << ":" << pev->eventType << endl;
    switch (pev->eventType) {
        case EVENT_NI_RECEIVE: {
            message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
            // search message id in eventFuncMap
            multimap<int,eventFunc2>::iterator im2 = eventFuncMap2.find(message->type);
            if (im2 != eventFuncMap2.end()) {
                P2PNetworkInterface *recv_interface = message->destinationInterface;
                (*im2).second(message,recv_interface);
            } else {
                OUTPUT << "ERROR: message Id #"<< message->type << " unknown!" << endl;
            }
        } break;

        case EVENT_ADD_NEIGHBOR: {
            OUTPUT << "ADD_NEIGHBOR" << endl;
            //startup();
        } break;

        case EVENT_TAP: {
            int face = (std::static_pointer_cast<TapEvent>(pev))->tappedFace;
            onTap(face);
        } break;

        case EVENT_HEXANODESMOTION_END: {
#ifdef verbose
            info.str("");
            info << "rec.: EVENT_MOTION_END";
            scheduler->trace(info.str(),hostBlock->blockId);
#endif
            onMotionEnd();
        }  break;
    }
}

bool HexanodesBlockCode::canMove(Hexanodes::motionDirection dir) {
    auto module = (HexanodesBlock*)hostBlock;
    vector<HexanodesMotion*> tab = getWorld()->getAllMotionsForModule(module);
    auto ci=tab.begin();
    while (ci!=tab.end() && (*ci)->direction!=dir) {
        ci++;
    }
    return (ci!=tab.end());
}

void HexanodesBlockCode::moveTo(Hexanodes::motionDirection dir,uint32_t delay) {
    auto module = (HexanodesBlock*)hostBlock;
    vector<HexanodesMotion*> tab = getWorld()->getAllMotionsForModule(module);
    auto ci=tab.begin();
    while (ci!=tab.end() && (*ci)->direction!=dir) {
        ci++;
    }
    if (ci!=tab.end()) {
        Cell3DPosition destination = (*ci)->getFinalPos(module->position);
        auto orient = (*ci)->getFinalOrientation(module->orientationCode);
        scheduler->schedule(new HexanodesMotionStartEvent(scheduler->now()+delay, module,destination,orient));
    }

}