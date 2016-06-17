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
#include "utils.h"

using namespace BaseSimulator::utils;
using namespace std;

namespace BaseSimulator {

    class World {
	boost::interprocess::interprocess_mutex mutex_gl;

    protected:
	static World *world;
	static vector<GlBlock*>tabGlBlocks;
	static map<int, BuildingBlock*>buildingBlocksMap;
	GlBlock *selectedBlock;
	GLushort numSelectedFace;
	GLuint numSelectedBlock;
	GLint menuId;
	GLfloat blockSize[3];

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

	/**
	 * Returns a boolean indicating if a block can be added to face #numSelectedFace
	 *  of block identified by numSelectedBlock
	 *
	 * @param numSelectedBlock id of selected block
	 * @param numSelectedFace id of face to consider
	 * @return true if corresponding cell is free and inside the grid, false otherwise
	 */
	virtual bool canAddBlockToFace(int numSelectedBlock, int numSelectedFace) { return true; };

	virtual BuildingBlock* getBlockById(int bId);
	virtual void updateGlData(BuildingBlock *bb);
	virtual void updateGlData(BuildingBlock*blc, Vector3D &p);
	virtual GlBlock* getSelectedBlock() { return selectedBlock; };
	inline GlBlock* setSelectedBlock(int n) { return (selectedBlock=(n>=0)?tabGlBlocks[n]:NULL); };
	virtual void setSelectedFace(int n) {};
	inline GlBlock* getBlockByNum(int n) { return tabGlBlocks[n]; };
	inline int getNbBlocks() { return buildingBlocksMap.size(); };
	void lock();
	void unlock();
	virtual void glDraw() {};
	virtual void glDrawId() {};
	virtual void glDrawIdByMaterial() {};

	/**
	 * Displays an interactive popup menu at coordinates (ix, iy)
	 *
	 * @param ix x coordinate of popup menu display location
	 * @param iy y coordinate of popup menu display location
	 */
	virtual void createPopupMenu(int ix, int iy);
	/**
	 * Creates a new help window at a fixed location of the screen
	 */
	void createHelpWindow();

	virtual Camera *getCamera() { return NULL; };
	virtual void menuChoice(int) {};
	virtual void exportConfiguration() {};
	virtual inline BuildingBlock* getMenuBlock() {
	    return getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
	};

	virtual void loadTextures(const string &str) { };

	inline void setBlocksSize(float *siz)
	    { blockSize[0] = siz[0]; blockSize[1] = siz[1]; blockSize[2] = siz[2]; };

	string generateConfigName();

	/* Notify the bId block that these events happened */

	/**
	 * Schedules a tap event for block with id bId, at time date.
	 *
	 * @param date the date at which the tap event must be consumed
	 * @param bId the id of the target block
	 */
	void tapBlock(uint64_t date, int bId);
	/**
	 * Schedules an accelerometer update event for block with id bId, at time date.
	 *
	 * @param date the date at which the accel event must be consumed
	 * @param bId the id of the target block
	 */
	virtual void accelBlock(uint64_t date, int bId, int x, int y, int z) {};
	/**
	 * Schedules a shake event for block with id bId, at time date.
	 *
	 * @param date the date at which the shake event must be consumed
	 * @param bId the id of the target block
	 */
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
