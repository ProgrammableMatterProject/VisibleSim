/*!
  * \file oktenGlBlock.h
  * \brief okten Block
  * \date 05/03/2015
  * \author Benoît Piranda
  */

#ifndef OKTENGLBLOCK_H_
#define OKTENGLBLOCK_H_
#include <string>
#include <objLoader.h>
#include "matrix44.h"
#include "glBlock.h"

namespace Okten {
class OktenGlBlock:public GlBlock {
protected :
public :
    Matrix mat{};
    uint8_t tabPosConnectors[6];

	OktenGlBlock(bID id);
	virtual ~OktenGlBlock() {};

	void glDraw(ObjLoader::ObjLoader *ptrObj);
	void glDrawConnectors(ObjLoader::ObjLoader *ptrObj);
	void glDrawIdConnectors(ObjLoader::ObjLoader *ptrObj, int &n);
	void glDrawId(ObjLoader::ObjLoader *ptrObj, int &n);
	void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj, int &n);
};
}
#endif /* OKTENGLBLOCK_H_ */
