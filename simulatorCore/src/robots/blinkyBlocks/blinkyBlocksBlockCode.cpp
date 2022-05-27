/*
 * blinkyBlocksBlockCode.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "blinkyBlocksBlockCode.h"
#include "../../utils/trace.h"

using namespace std;

namespace BlinkyBlocks {

    BlinkyBlocksBlockCode::BlinkyBlocksBlockCode(BlinkyBlocksBlock *host) : BlockCode(host) {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << "BlinkyBlocksBlockCode constructor" << endl;
#endif
    }

    BlinkyBlocksBlockCode::~BlinkyBlocksBlockCode() {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << "BlinkyBlocksBlockCode destructor" << endl;
#endif
    }


}
