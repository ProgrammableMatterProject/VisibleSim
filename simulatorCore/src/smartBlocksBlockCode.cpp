/*
 * blinkyBlocksBlockCode.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "smartBlocksBlockCode.h"

using namespace std;

namespace SmartBlocks {

SmartBlocksBlockCode::SmartBlocksBlockCode(SmartBlocksBlock *host):BlockCode(host) {
	OUTPUT << "SmartBlocksBlockCode constructor" << endl;
}

SmartBlocksBlockCode::~SmartBlocksBlockCode() {
	OUTPUT << "SmartBlocksBlockCode destructor" << endl;
}

}
