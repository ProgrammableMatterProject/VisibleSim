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
	OUTPUT << "MultiRobotsBlockCode constructor" << endl;
}

MultiRobotsBlockCode::~MultiRobotsBlockCode() {
	OUTPUT << "MultiRobotsBlockCode destructor" << endl;
}

}
