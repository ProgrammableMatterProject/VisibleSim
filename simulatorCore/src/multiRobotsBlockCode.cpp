/*
 * @file multiRobotsBlockCode.cpp
 *
 *  Created on: 14/07/2016
 *      Author: pthalamy
 */

#include <iostream>

#include "multiRobotsBlockCode.h"
#include "trace.h"

using namespace std;

namespace MultiRobots {

MultiRobotsBlockCode::MultiRobotsBlockCode(MultiRobotsBlock *host):BlockCode(host) {
#ifdef DEBUG_OBJECT_LIFECYCLE
	OUTPUT << "MultiRobotsBlockCode constructor" << endl;
#endif
}

MultiRobotsBlockCode::~MultiRobotsBlockCode() {
#ifdef DEBUG_OBJECT_LIFECYCLE    
	OUTPUT << "MultiRobotsBlockCode destructor" << endl;
#endif
}

}
