/*
 * world.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef WORLD_H_
#define WORLD_H_

#include <random>
#include <iostream>
#include <map>
#include <vector>
#include <mutex>

#include <boost/interprocess/sync/interprocess_mutex.hpp>

#include "assert.h"
#include "buildingBlock.h"
#include "glBlock.h"
#include "trace.h"
#include "utils.h"
#include "lattice.h"
#include "scheduler.h"

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
    Camera *camera;

    World(int argc, char *argv[]);
    virtual ~World();
public:
    Lattice *lattice;

    static World* getWorld() {
        assert(world != NULL);
        return(world);
    }

    /**
     * @brief Sets the value of static world variable
     * @param _world pointer to a world instance
     */
    static void setWorld(World *_world) {
        world = _world;
    }

    static void deleteWorld() {
        delete(world);
        world=NULL;
    }

    map<int, BuildingBlock*>& getMap() {
        return buildingBlocksMap;
    }

    /**
     * @brief Returns the number of blocks in the world
     * @return Number of blocks in the world
     */
    inline int getSize() { return buildingBlocksMap.size(); };
    /**
     * @brief Prints a string identifying the world to OUTPUT
     */
    inline void printInfo() { OUTPUT << "I'm a World" << endl; };
    /**
     * Returns a boolean indicating if a block can be added to face #numSelectedFace
     *  of block identified by numSelectedBlock
     *
     * @param numSelectedBlock id of selected block
     * @param numSelectedFace id of face to consider
     * @return true if corresponding cell is free and inside the grid, false otherwise
     */
    bool canAddBlockToFace(int numSelectedBlock, int numSelectedFace);

    virtual BuildingBlock* getBlockById(int bId);
    /**
     * @brief Updates color and position of glBlock associated with block bb
     *
     * @param bb : Block to update
     */
    virtual void updateGlData(BuildingBlock *bb);
    /**
     * @brief Set position p to glBlock associated with block blc
     *
     * Used when glBlocks and their corresponding BuildingBlock have different positions,
     *  as it is the case during motion events
     *
     * @param blc : Block to update
     * @param p : Position to set to blc's glBlock
     */
    virtual void updateGlData(BuildingBlock*blc, Vector3D &p);

    virtual GlBlock* getSelectedBlock() { return selectedBlock; };
    inline GlBlock* setSelectedBlock(int n) { return (selectedBlock=(n>=0)?tabGlBlocks[n]:NULL); };

    virtual void setSelectedFace(int n) = 0;

    inline GlBlock* getBlockByNum(int n) { return tabGlBlocks[n]; };
    inline int getNbBlocks() { return buildingBlocksMap.size(); };
    /**
     * @brief Locks the world mutex to avoid concurrency issues with the gl process
     */
    inline void lock() { mutex_gl.lock(); };
    /**
     * @brief Unlocks the world mutex to re-enable access from the gl process
     */
    inline void unlock() { mutex_gl.unlock(); };
    virtual void glDraw() {};
    virtual void glDrawId() {};
    virtual void glDrawIdByMaterial() {};
    /**
     * @brief Linearly scans the grid for blocks and calls linkBlock to connect the interfaces of neighbors
     *
     */
    void linkBlocks();
    /**
     * @brief Updates the neighborhood of all alive neighbors of cell pos
     * @param pos : Position of the block whose neighbors need an update
     */
    void linkNeighbors(const Cell3DPosition &pos);
    /**
     * @brief Connects block on grid cell pos to its neighbor
     * @param pos : Position of the block to connect
     */
    virtual void linkBlock(const Cell3DPosition &pos) = 0;
    /**
     * @brief Displays an interactive popup menu at coordinates (ix, iy)
     *
     * @param ix x coordinate of popup menu display location
     * @param iy y coordinate of popup menu display location
     */
    virtual void createPopupMenu(int ix, int iy);
    /**
     * @brief Creates a new help window at a fixed location of the screen
     */
    void createHelpWindow();

    virtual Camera *getCamera() { return camera; };
    virtual void menuChoice(int) {};
    virtual void exportConfiguration() {};
    virtual inline BuildingBlock* getMenuBlock() {
        return getBlockById(tabGlBlocks[numSelectedBlock]->blockId);
    };

    virtual void loadTextures(const string &str) { };

    string generateConfigName();

    /**
     * @copydoc
     */
    void tapBlock(uint64_t date, int bId);

    /* Stop the block execution */
    void stopSimulation();

    void generateIds(int n, int *ids);
};

inline void deleteWorld() {
    World::deleteWorld();
}

static inline World* getWorld() { return(World::getWorld()); }

static inline void setWorld(World* _world) { World::setWorld(_world); }

} // BaseSimulator namespace

#endif /* WORLD_H_ */
