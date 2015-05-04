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

static void swap(int* a, int* b)
{
 int temp = *a;
 *a = *b;
 *b = temp;
}

void World::generateIds(int n, int* ids) {
	int a = 0, b = 0;
	
	//struct timespec t;
	//clock_gettime(CLOCK_REALTIME, &t);
	// not implemented in macos
	//boost::rand48 generator = boost::rand48(t.tv_nsec);
	
	boost::rand48 generator = boost::rand48(time(NULL));
	
	for (int i = 0; i < n; i++) {
		ids[i] = i+1;
	}
	
	if (n==1) {
		return;
	}
		
	// randomly switch 2n times
	for (int i = 0; i < n*2; i++) {
		do {
			a = generator() % n;
			b = generator() % n;
		} while ((a == b));
		swap(&ids[a], &ids[b]);
	}
}


} // BaseSimulator namespace
