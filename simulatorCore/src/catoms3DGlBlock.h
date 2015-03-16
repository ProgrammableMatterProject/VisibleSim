/*
 * catoms3DGlBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS3DGLBLOCK_H_
#define CATOMS3DGLBLOCK_H_
#include <string>
#include <objLoader.h>
#include "glBlock.h"
#include "catoms3DCapabilities.h"

namespace Catoms3D {
class Catoms3DGlBlock:public GlBlock {
protected :
public :
    GLfloat theta,phi,psi; //!< spherical coordinates angles in degree

	Catoms3DGlBlock(int id);
	virtual ~Catoms3DGlBlock();
	virtual string getInfo();

	void setAngles(float t,float p,float f);

	void glDraw(ObjLoader::ObjLoader *ptrObj);
	void glDrawId(ObjLoader::ObjLoader *ptrObj,int &n);
	void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n);
};
}
#endif /* CATOMS3DGLBLOCK_H_ */
