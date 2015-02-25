/*
 * blockCode.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "blockCode.h"
#include "trace.h"

using namespace std;

namespace BaseSimulator {

BlockCode::BlockCode(BuildingBlock *host) {
	OUTPUT << "BlockCode constructor" << endl;
	hostBlock = host;
	availabilityDate = 0;
}

BlockCode::~BlockCode() {
	OUTPUT << "BlockCode destructor" << endl;
}

} // BaseSimulator namespace
