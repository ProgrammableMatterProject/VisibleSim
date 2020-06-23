/*
 * slidingCubesBlockCode.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "slidingCubesBlockCode.h"
#include "../../utils/trace.h"

using namespace std;

namespace SlidingCubes {
SlidingCubesBlockCode::SlidingCubesBlockCode(SlidingCubesBlock *host):BlockCode(host) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "SlidingCubesBlockCode constructor" << endl;
#endif
}

SlidingCubesBlockCode::~SlidingCubesBlockCode() {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "SlidingCubesBlockCode destructor" << endl;
#endif
}

void SlidingCubesBlockCode::processLocalEvent(EventPtr pev) {
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
