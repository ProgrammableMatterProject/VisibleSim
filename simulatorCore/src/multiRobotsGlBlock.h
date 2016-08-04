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

	virtual string getInfo();

	void glDraw(ObjLoader::ObjLoader *ptrObj);
	void glDrawId(ObjLoader::ObjLoader *ptrObj,int &n);
	void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n);
};

}

#endif /* MULTIROBOTSGLBLOCK_H_ */
