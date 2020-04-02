/**
 * @file   nodeDemoBlockCode.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:15:26 2019
 *
 * @brief
 *
 *
 */

#include "hexanodesDemoBlockCode.hpp"

#include "robots/hexanodes/hexanodesWorld.h"

#include "events/scheduler.h"
#include "events/events.h"
#include "utils/trace.h"
#include "robots/hexanodes/hexanodesMotionEvents.h"
#include "robots/hexanodes/hexanodesMotionEngine.h"

using namespace Hexanodes;

HexanodesDemoBlockCode::HexanodesDemoBlockCode(HexanodesBlock *host):HexanodesBlockCode(host) {
    scheduler = getScheduler();
    node = (HexanodesBlock*)hostBlock;
}

HexanodesDemoBlockCode::~HexanodesDemoBlockCode() {
}

// Function called by the module upon initialization
void HexanodesDemoBlockCode::startup() {
    HexanodesWorld *wrl = Hexanodes::getWorld();
    // Dummy translation example
    if (node->blockId == 1) {
        // turn clockwise if possible !
        vector<HexanodesMotion*> tab = wrl->getAllMotionsForModule(node);
        console << "#motion=" << tab.size() << "\n";
        vector<HexanodesMotion*>::const_iterator ci=tab.begin();
        while (ci!=tab.end() && (*ci)->direction!=motionDirection::CW) {
                ci++;
        }
        if (ci!=tab.end()) {
            Cell3DPosition destination = (*ci)->getFinalPos(node->position);
            previousPivot = (*ci)->getToConId();
            cout << "previousPivot=" << previousPivot << " md=" << (*ci)->direction << endl;
            scheduler->schedule(new HexanodesMotionStartEvent(scheduler->now()+1000000, node,destination,previousPivot));
        }
    }
}

void HexanodesDemoBlockCode::onMotionEnd() {
    // turn clockwise from previousPivot attachment
    HexanodesWorld *wrl = Hexanodes::getWorld();
    vector<HexanodesMotion*> tab = wrl->getAllMotionsForModule(node);
    vector<HexanodesMotion*>::const_iterator ci=tab.begin();
    while (ci!=tab.end() && !((*ci)->direction==motionDirection::CW && (*ci)->fromConId==previousPivot)) {
        ci++;
    }
    if (ci!=tab.end()) {
        Cell3DPosition destination = (*ci)->getFinalPos(node->position);
        previousPivot = (*ci)->getToConId();
        cout << "previousPivot=" << previousPivot << " md=" << (*ci)->direction << endl;
        scheduler->schedule(new HexanodesMotionStartEvent(scheduler->now()+1000000, node,destination,previousPivot));
    }
}
