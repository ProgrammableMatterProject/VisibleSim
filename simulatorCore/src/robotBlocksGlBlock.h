/*
 * robotBlocksGlBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef ROBOTBLOCKSGLBLOCK_H_
#define ROBOTBLOCKSGLBLOCK_H_
#include <string>
#include <objLoader.h>
#include "glBlock.h"

namespace RobotBlocks {
class RobotBlocksGlBlock:public GlBlock {
protected :
	int nextId,prevId;
public :
	RobotBlocksGlBlock(bID id);
	virtual ~RobotBlocksGlBlock();
	virtual string getInfo();

	virtual void setPrevNext(int p,int n);

	void glDraw(ObjLoader::ObjLoader *ptrObj);
	void glDrawId(ObjLoader::ObjLoader *ptrObj,int &n);
	void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n);
};
}
#endif /* ROBOTBLOCKSGLBLOCK_H_ */
