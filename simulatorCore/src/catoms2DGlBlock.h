/*
 * catoms2DGlBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS2DGLBLOCK_H_
#define CATOMS2DGLBLOCK_H_

#include <string>
#include <objLoader.h>

#include "glBlock.h"

namespace Catoms2D {
class Catoms2DGlBlock:public GlBlock {
protected :
public :
    double angle = 0; //!< orientation angle in degree around Y axis

	Catoms2DGlBlock(bID id) : GlBlock(id) {};
	virtual ~Catoms2DGlBlock() {};

	inline void setAngle(double a) {angle=a;};

	void glDraw(ObjLoader::ObjLoader *ptrObj);
};
}
#endif /* CATOMS2DGLBLOCK_H_ */
