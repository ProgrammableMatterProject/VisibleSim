/*
 * world.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef WORLD_H_
#define WORLD_H_

#include <iostream>
#include <map>
#include <vector>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include "assert.h"
#include "buildingBlock.h"
#include "glBlock.h"
#include "trace.h"

using namespace std;

namespace BaseSimulator {

    class World {
	boost::interprocess::interprocess_mutex mutex_gl;

    protected:
	static World *world;
	static vector<GlBlock*>tabGlBlocks;
	static map<int, BuildingBlock*>buildingBlocksMap;
	GlBlock *selectedBlock;

	World();
	virtual ~World();

    public:
	
	static World* getWorld() {
            assert(world != NULL);
            return(world);
	}
	
	static void deleteWorld() {
            delete(world);
            world=NULL;
	}
	
	map<int, BuildingBlock*>& getMap() {
            return buildingBlocksMap;
	}

	int getSize() {
            return buildingBlocksMap.size();
	}

	void printInfo() {
            OUTPUT << "I'm a World" << endl;
	}
	
	virtual BuildingBlock* getBlockById(int bId);

        inline GlBlock* getSelectedBlock() { return selectedBlock; };
        inline GlBlock* setSelectedBlock(int n) { return (selectedBlock=(n>=0)?tabGlBlocks[n]:NULL); };
        virtual void setSelectedFace(int n) {};
        inline GlBlock* getBlockByNum(int n) { return tabGlBlocks[n]; };
        inline int getNbBlocks() { return buildingBlocksMap.size(); };
        void lock();
        void unlock();
        virtual void glDraw() {};
        virtual void glDrawId() {};
        virtual void glDrawIdByMaterial() {};
        virtual void createPopupMenu(int ix,int iy) {};
        virtual void createHelpWindow() {};
        virtual Camera *getCamera() { return NULL; };
        virtual void menuChoice(int) {};
        /* Notify the bId block that these events happened */
        void tapBlock(uint64_t date, int bId);        
        virtual void accelBlock(uint64_t date, int bId, int x, int y, int z) {};
        virtual void shakeBlock(uint64_t date, int bId, int f) {};
        /* Stop the block execution */
        void stopSimulation();
	
        void generateIds(int n, int *ids);
    };

    inline void deleteWorld() {
	World::deleteWorld();
    }

    inline World* getWorld() { return(World::getWorld()); }

} // BaseSimulator namespace

#endif /* WORLD_H_ */
