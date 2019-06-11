/*
 * @file multiRobotsGlBlock.h
 *
 *  Created on: 14/07/2016
 *      Author: pthalamy
 */

#ifndef MULTIROBOTSGLBLOCK_H_
#define MULTIROBOTSGLBLOCK_H_

#include <string>
#include <objLoader.h>

#include "glBlock.h"

namespace MultiRobots {

class MultiRobotsGlBlock:public GlBlock {
protected :
public :
    MultiRobotsGlBlock(bID id) : GlBlock(id) {};
    virtual ~MultiRobotsGlBlock() {};

    virtual void glDraw(ObjLoader::ObjLoader *ptrObj) override;
};

}

#endif /* MULTIROBOTSGLBLOCK_H_ */
