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
	cout << "SmartBlocksBlockCode constructor" << endl;
}

SmartBlocksBlockCode::~SmartBlocksBlockCode() {
	cout << "SmartBlocksBlockCode destructor" << endl;
}
/*
SmartBlocksBlockCode* SmartBlocksBlockCode::buildNewBlockCode(SmartBlocksBlock *host) {
	return(new SmartBlocksBlockCode(host));
}
*/

}
