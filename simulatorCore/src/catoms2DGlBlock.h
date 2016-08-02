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
    double angle; //!< orientation angle in degree around Y axis

	Catoms2DGlBlock(int id);
	virtual ~Catoms2DGlBlock();
	virtual string getInfo();

	inline void setAngle(double a) {angle=a;};

	void glDraw(ObjLoader::ObjLoader *ptrObj);
	void glDrawId(ObjLoader::ObjLoader *ptrObj,int &n);
	void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n);
};
}
#endif /* CATOMS2DGLBLOCK_H_ */
