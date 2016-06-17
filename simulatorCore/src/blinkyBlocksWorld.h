/*
 * blinkyBlocksWorld.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef BLINKYBLOCKSWORLD_H_
#define BLINKYBLOCKSWORLD_H_

#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "blinkyBlocksBlock.h"
#include "objLoader.h"
#include <boost/asio.hpp>
#include "trace.h"
#include <blinkyBlocksScenario.h>
#include <vector>

#include "grid3D.h"

namespace BlinkyBlocks {

class BlinkyBlocksWorld : public BaseSimulator::World {
protected:
    Grid3D<BuildingBlock*> *grid;

    int gridSize[3];

    BlinkyBlocksBlock **gridPtrBlocks;
    Camera *camera;
    GLuint idTextureWall;
    GLint menuId;
    ObjLoader::ObjLoader *objBlock,*objBlockForPicking,*objRepere;

    vector<ScenarioEvent*> tabEvents;

    BlinkyBlocksWorld(int slx,int sly,int slz, int argc, char *argv[]);
    virtual ~BlinkyBlocksWorld();
    inline BlinkyBlocksBlock* getGridPtr(int ix,int iy,int iz) { return gridPtrBlocks[ix+(iy+iz*gridSize[1])*gridSize[0]]; };
    inline void setGridPtr(int ix,int iy,int iz,BlinkyBlocksBlock *ptr) { gridPtrBlocks[ix+(iy+iz*gridSize[1])*gridSize[0]]=ptr; };
public:
    static void createWorld(int slx,int sly,int slz, int argc, char *argv[]);
    static void deleteWorld();
    static BlinkyBlocksWorld* getWorld() {
	assert(world != NULL);
	return((BlinkyBlocksWorld*)world);
    }

    inline int* getGridSize() { return gridSize; };
    inline float* getBlockSize() { return blockSize; };

    void printInfo() {
	OUTPUT << "I'm a BlinkyBlocksWorld" << endl;
    }

    virtual BlinkyBlocksBlock* getBlockById(int bId) {
	return((BlinkyBlocksBlock*)World::getBlockById(bId));
    }

    virtual void addBlock(int blockId, BlinkyBlocksBlockCode *(*blinkyBlockCodeBuildingFunction)(BlinkyBlocksBlock*), const Cell3DPosition &pos, const Color &col);
    void deleteBlock(BlinkyBlocksBlock *bb);

    void linkBlocks();
    void loadTextures(const string &str);
    virtual void glDraw();
    virtual void glDrawId();
    virtual void glDrawIdByMaterial();
    inline virtual Camera *getCamera() { return camera; };
    virtual void setSelectedFace(int n);
    virtual void menuChoice(int n);
    virtual void exportConfiguration();
    virtual inline BuildingBlock* getMenuBlock() { return World::getMenuBlock(); };

    /* Sends the appropriate message (tap, ...) to the VM associated to bId block (through the scheduler)*/
    void accelBlock(uint64_t date, int bId, int x, int y, int z);
    void shakeBlock(uint64_t date, int bId, int f);
    void stopBlock(uint64_t date, int bId);

    virtual bool canAddBlockToFace(int numSelectedBlock, int numSelectedFace);

    void addScenarioEvent(ScenarioEvent *ev) { tabEvents.push_back(ev); };

    // Prints information about the blocks
    void dump();

};

std::ostream& operator<<(std::ostream &stream, BlinkyBlocksBlock const& bb);

inline void createWorld(int slx,int sly,int slz, int argc, char *argv[]) {
    BlinkyBlocksWorld::createWorld(slx,sly,slz, argc,argv);
}

inline void deleteWorld() {
    BlinkyBlocksWorld::deleteWorld();
}

inline BlinkyBlocksWorld* getWorld() { return(BlinkyBlocksWorld::getWorld()); }

} // BlinkyBlocks namespace

#endif /* BLINKYBLOCKSWORLD_H_ */
