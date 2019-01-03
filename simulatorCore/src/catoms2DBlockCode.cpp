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
#ifdef DEBUG_OBJECT_LIFECYCLE
	OUTPUT << "Catoms2DBlockCode constructor" << endl;
#endif
}

Catoms2DBlockCode::~Catoms2DBlockCode() {
#ifdef DEBUG_OBJECT_LIFECYCLE
	OUTPUT << "Catoms2DBlockCode destructor" << endl;
#endif
}

}
