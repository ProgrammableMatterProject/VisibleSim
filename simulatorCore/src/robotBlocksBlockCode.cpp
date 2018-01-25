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

}
