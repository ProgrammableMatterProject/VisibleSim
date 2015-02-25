/*
 * multiCoresWorld.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef MULTICORESWORLD_H_
#define MULTICORESWORLD_H_

#include "world.h"
#include "multiCoresBlockCode.h"

namespace MultiCores {

class MultiCoresWorld : public BaseSimulator::World {
protected:
	MultiCoresWorld();
	virtual ~MultiCoresWorld();

public:
	static void createWorld();
	static void deleteWorld();
	static MultiCoresWorld* getWorld() {
		assert(world != NULL);
		return((MultiCoresWorld*)world);
	}

	void printInfo() {
		cout << "I'm a MultiCoresWorld" << endl;
	}

	virtual MultiCoresBlock* getBlockById(int bId) {
		return((MultiCoresBlock*)World::getBlockById(bId));
	}

	virtual void addBlock(int blockId, MultiCoresBlockCode *(*multiCoreBlockCodeBuildingFunction)(MultiCoresBlock*),float posX, float posY, float posZ, float colorR, float colorG, float colorB);

};

inline void createWorld() {
	MultiCoresWorld::createWorld();
}

inline void deleteWorld() {
	MultiCoresWorld::deleteWorld();
}

inline MultiCoresWorld* getWorld() { return(MultiCoresWorld::getWorld()); }

} // MultiCores namespace

#endif /* MULTICORESWORLD_H_ */
