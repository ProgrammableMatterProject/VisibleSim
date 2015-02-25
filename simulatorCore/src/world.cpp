/*
 * world.cpp
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#include <stdlib.h>
#include "world.h"
#include "trace.h"

using namespace std;

namespace BaseSimulator {

World *World::world=NULL;
map<int, BuildingBlock*>World::buildingBlocksMap;
vector <GlBlock*>World::tabGlBlocks;

World::World() {
	OUTPUT << "World constructor" << endl;
	selectedBlock = NULL;
	if (world == NULL) {
		world = this;
	} else {
		ERRPUT << "\033[1;31m" << "Only one World instance can be created, aborting !" << "\033[0m" << endl;
		exit(EXIT_FAILURE);
	}
}

World::~World() {
	//MODIF NICO
	std::map<int, BuildingBlock*>::iterator it;
	for( it = buildingBlocksMap.begin() ; it != buildingBlocksMap.end() ; ++it) {
		delete it->second;
	}
	//FIN MODIF NICO

	std::vector<GlBlock*>::const_iterator cit=tabGlBlocks.begin();
	while (cit!=tabGlBlocks.end()) {
		delete *cit;
		cit++;
	}
	OUTPUT << "World destructor" << endl;
}

BuildingBlock* World::getBlockById(int bId) {
	map<int, BuildingBlock*>::iterator it;
	it = buildingBlocksMap.find(bId);
	if (it == buildingBlocksMap.end()) {
		return(NULL);
	} else {
		return(it->second);
	}
}

void World::lock() {
	mutex_gl.lock();
}

void World::unlock() {
	mutex_gl.unlock();
}

void World::stopSimulation() {
	map<int, BuildingBlock*>::iterator it;
	for( it = buildingBlocksMap.begin() ; it != buildingBlocksMap.end() ; it++) {
		it->second->stop();
	}
}

} // BaseSimulator namespace
