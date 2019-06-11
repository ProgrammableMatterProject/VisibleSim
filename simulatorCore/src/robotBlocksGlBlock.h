/*
 * robotBlocksGlBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef ROBOTBLOCKSGLBLOCK_H_
#define ROBOTBLOCKSGLBLOCK_H_
#include <string>
#include <sstream>
#include <objLoader.h>
#include "glBlock.h"

namespace RobotBlocks {
class RobotBlocksGlBlock:public GlBlock {
protected :
    int nextId = 0;
    int prevId = 0;
public :
    string popupString;

    RobotBlocksGlBlock(bID id) : GlBlock(id) {};
    virtual ~RobotBlocksGlBlock() {};

    virtual void setPrevNext(int p,int n);
    virtual string getPopupInfo() override;

    void glDraw(ObjLoader::ObjLoader *ptrObj) override;
};
}
#endif /* ROBOTBLOCKSGLBLOCK_H_ */
