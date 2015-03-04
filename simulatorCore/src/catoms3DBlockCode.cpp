/*
 * catoms3DBlockCode.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "catoms3DBlockCode.h"
#include "trace.h"

using namespace std;

namespace Catoms3D {

Catoms3DBlockCode::Catoms3DBlockCode(Catoms3DBlock *host):BlockCode(host) {
	OUTPUT << "Catoms3DBlockCode constructor" << endl;
}

Catoms3DBlockCode::~Catoms3DBlockCode() {
	OUTPUT << "Catoms3DBlockCode destructor" << endl;
}
/*
Catoms3DBlockCode* Catoms3DBlockCode::buildNewBlockCode(Catoms3DBlock *host) {
	return(new Catoms3DBlockCode(host));
}
*/

}
