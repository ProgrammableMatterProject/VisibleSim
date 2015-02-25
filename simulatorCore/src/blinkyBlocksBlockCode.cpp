/*
 * blinkyBlocksBlockCode.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "blinkyBlocksBlockCode.h"
#include "trace.h"

using namespace std;

namespace BlinkyBlocks {

BlinkyBlocksBlockCode::BlinkyBlocksBlockCode(BlinkyBlocksBlock *host):BlockCode(host) {
	OUTPUT << "BlinkyBlocksBlockCode constructor" << endl;
	currentLocalDate = 0; // mode fastest
	hasWork = true; // mode fastest
	polling = false; // mode fastest
}

BlinkyBlocksBlockCode::~BlinkyBlocksBlockCode() {
	OUTPUT << "BlinkyBlocksBlockCode destructor" << endl;
}
/*
BlinkyBlocksBlockCode* BlinkyBlocksBlockCode::buildNewBlockCode(BlinkyBlocksBlock *host) {
	return(new BlinkyBlocksBlockCode(host));
}
*/

}
