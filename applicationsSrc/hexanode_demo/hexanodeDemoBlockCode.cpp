/**
 * @file   nodeDemoBlockCode.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:15:26 2019
 *
 * @brief
 *
 *
 */

#include "hexanodeDemoBlockCode.hpp"

#include "hexanodeWorld.h"

#include "scheduler.h"
#include "events.h"
#include "trace.h"
#include "hexanodeMotionEvents.h"
#include "hexanodeMotionEngine.h"

using namespace Hexanode;

HexanodeDemoBlockCode::HexanodeDemoBlockCode(HexanodeBlock *host):HexanodeBlockCode(host) {
    scheduler = getScheduler();
    node = (HexanodeBlock*)hostBlock;
}

HexanodeDemoBlockCode::~HexanodeDemoBlockCode() {
}

// Function called by the module upon initialization
void HexanodeDemoBlockCode::startup() {
	HexanodeWorld *wrl = Hexanode::getWorld();
	// Dummy translation example
	if (node->blockId == 1) {
		// turn clockwise if possible !
		vector<HexanodeMotion*> tab = wrl->getAllMotionsForModule(node);
		console << "#motion=" << tab.size() << "\n";
		vector<HexanodeMotion*>::const_iterator ci=tab.begin();
		while (ci!=tab.end() && (*ci)->direction!=motionDirection::CW) {
				ci++;
		}
		if (ci!=tab.end()) {
			Cell3DPosition destination = (*ci)->getFinalPos(node->position);
			previousPivot = (*ci)->getToConId();
			cout << "previousPivot=" << previousPivot << " md=" << (*ci)->direction << endl;
			scheduler->schedule(new HexanodeMotionStartEvent(scheduler->now()+1000000, node,destination,previousPivot));
		}
	}
}

void HexanodeDemoBlockCode::onMotionEnd() {
	// turn clockwise from previousPivot attachment
	HexanodeWorld *wrl = Hexanode::getWorld();
	vector<HexanodeMotion*> tab = wrl->getAllMotionsForModule(node);
	vector<HexanodeMotion*>::const_iterator ci=tab.begin();
	while (ci!=tab.end() && !((*ci)->direction==motionDirection::CW && (*ci)->fromConId==previousPivot)) {
		ci++;
	}
	if (ci!=tab.end()) {
		Cell3DPosition destination = (*ci)->getFinalPos(node->position);
		previousPivot = (*ci)->getToConId();
		cout << "previousPivot=" << previousPivot << " md=" << (*ci)->direction << endl;
		scheduler->schedule(new HexanodeMotionStartEvent(scheduler->now()+1000000, node,destination,previousPivot));
	}
}
