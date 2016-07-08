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
	OUTPUT << "RobotBlocksBlockCode constructor" << endl;
}

RobotBlocksBlockCode::~RobotBlocksBlockCode() {
	OUTPUT << "RobotBlocksBlockCode destructor" << endl;
}

}
