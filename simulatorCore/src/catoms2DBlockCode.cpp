/*
 * catoms2DBlockCode.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "catoms2DBlockCode.h"
#include "trace.h"

using namespace std;

namespace Catoms2D {

Catoms2DBlockCode::Catoms2DBlockCode(Catoms2DBlock *host):BlockCode(host) {
	OUTPUT << "Catoms2DBlockCode constructor" << endl;
}

Catoms2DBlockCode::~Catoms2DBlockCode() {
	OUTPUT << "Catoms2DBlockCode destructor" << endl;
}
/*
Catoms2DBlockCode* Catoms2DBlockCode::buildNewBlockCode(Catoms2DBlock *host) {
	return(new Catoms2DBlockCode(host));
}
*/

}
