/*
 * multiCoresBlockCode.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#include <iostream>
#include "multiCoresBlockCode.h"

using namespace std;

namespace MultiCores {

MultiCoresBlockCode::MultiCoresBlockCode(MultiCoresBlock *host):BlockCode(host) {
	cout << "MultiCoresBlockCode constructor" << endl;
}

MultiCoresBlockCode::~MultiCoresBlockCode() {
	cout << "MultiCoresBlockCode destructor" << endl;
}

/*
MultiCoresBlockCode* MultiCoresBlockCode::buildNewBlockCode(MultiCoresBlock *host) {
	return(new MultiCoresBlockCode(host));
}
*/

}
