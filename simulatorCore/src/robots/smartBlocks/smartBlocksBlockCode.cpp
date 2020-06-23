/*
 * blinkyBlocksBlockCode.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "smartBlocksBlockCode.h"
#include "../../utils/trace.h"

using namespace std;

namespace SmartBlocks {
SmartBlocksBlockCode::SmartBlocksBlockCode(SmartBlocksBlock *host):BlockCode(host) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "SmartBlocksBlockCode constructor" << endl;
#endif
}

SmartBlocksBlockCode::~SmartBlocksBlockCode() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "SmartBlocksBlockCode destructor" << endl;
#endif
}

void SmartBlocksBlockCode::processLocalEvent(EventPtr pev) {
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
