/*
 * blinkyBlocksGlBlock.h
 *
 *  Created on: 05 juin 2013
 *      Author: ben
 */

#ifndef BLINKYBLOCKSGLBLOCK_H_
#define BLINKYBLOCKSGLBLOCK_H_
#include <string>
#include <objLoader.h>
#include "glBlock.h"

namespace BlinkyBlocks {
class BlinkyBlocksGlBlock:public GlBlock {
protected :
public :
	BlinkyBlocksGlBlock(int id);
	virtual ~BlinkyBlocksGlBlock();
	virtual string getInfo();

	void glDraw(ObjLoader::ObjLoader *ptrObj);
	void glDrawId(ObjLoader::ObjLoader *ptrObj,int &n);
	void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n);
};
}
#endif /* BLINKYBLOCKSGLBLOCK_H_ */
