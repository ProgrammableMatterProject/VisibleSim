/*
 * robotBlocksBlockCode.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "robotBlocksBlockCode.h"
#include "trace.h"

using namespace std;

namespace RobotBlocks {
RobotBlocksBlockCode::RobotBlocksBlockCode(RobotBlocksBlock *host):BlockCode(host) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "RobotBlocksBlockCode constructor" << endl;
#endif
}

RobotBlocksBlockCode::~RobotBlocksBlockCode() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "RobotBlocksBlockCode destructor" << endl;
#endif
}

void RobotBlocksBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;

    BlockCode::processLocalEvent(pev);

    switch (pev->eventType) {
        case EVENT_TRANSLATION_END: {
#ifdef verbose
            info.str("");
            info << "rec.: EVENT_MOTION_END";
            scheduler->trace(info.str(),hostBlock->blockId);
#endif
            onMotionEnd();
        }  break;
    }
}

}
