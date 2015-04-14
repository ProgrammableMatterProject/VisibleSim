/*
 * multiCoresWorld.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <stdlib.h>
#include "multiCoresWorld.h"
#include "multiCoresBlock.h"
#include "multiCoresScheduler.h"

using namespace std;

namespace MultiCores {

MultiCoresWorld::MultiCoresWorld() {
	cout << "MultiCoresWorld constructor" << endl;
}

MultiCoresWorld::~MultiCoresWorld() {
	cout << "MultiCoresWorld destructor" << endl;
}


void MultiCoresWorld::createWorld() {
	world = new MultiCoresWorld();
}

void MultiCoresWorld::deleteWorld() {
	delete((MultiCoresWorld*)world);
}

void MultiCoresWorld::addBlock(int blockId, MultiCoresBlockCode *(*multiCoreBlockCodeBuildingFunction)(MultiCoresBlock*),float posX, float posY, float posZ, float colorR, float colorG, float colorB) {
	MultiCoresBlock *multiCoresBlock;
	multiCoresBlock = new MultiCoresBlock(blockId,multiCoreBlockCodeBuildingFunction);
	buildingBlocksMap.insert(std::pair<int,BaseSimulator::BuildingBlock*>(multiCoresBlock->blockId, (BaseSimulator::BuildingBlock*)multiCoresBlock) );
	getScheduler()->schedule(new CodeStartEvent(getScheduler()->now(),multiCoresBlock));
}

} // MultiCores namespace
