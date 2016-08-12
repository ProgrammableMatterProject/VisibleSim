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
	int nextId = 0;
	int prevId = 0;
public :
	RobotBlocksGlBlock(bID id) : GlBlock(id) {};
	virtual ~RobotBlocksGlBlock() {};

	virtual void setPrevNext(int p,int n);

	void glDraw(ObjLoader::ObjLoader *ptrObj);
};
}
#endif /* ROBOTBLOCKSGLBLOCK_H_ */
