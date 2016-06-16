/*
 * robotBlocksWorld.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef ROBOTBLOCKSWORLD_H_
#define ROBOTBLOCKSWORLD_H_

#include "openglViewer.h"
#include "world.h"
#include "vector3D.h"
#include "robotBlocksBlock.h"
#include "robotBlocksCapabilities.h"
#include "objLoader.h"
#include <boost/asio.hpp>
#include "trace.h"
#include <vector>

namespace RobotBlocks {

class RobotBlocksWorld : public BaseSimulator::World {
protected:
	int gridSize[3];
	RobotBlocksBlock **gridPtrBlocks;
	Camera *camera;
	GLuint idTextureWall;
	GLint menuId;
	presence *targetGrid;
	RobotBlocksCapabilities *capabilities;
	ObjLoader::ObjLoader *objBlock,*objBlockForPicking,*objRepere;

	RobotBlocksWorld(int slx,int sly,int slz, int argc, char *argv[]);
	virtual ~RobotBlocksWorld();
	void linkBlock(int ix,int iy,int iz);

public:
	static void createWorld(int slx,int sly,int slz, int argc, char *argv[]);
	static void deleteWorld();
	static RobotBlocksWorld* getWorld() {
		assert(world != NULL);
		return((RobotBlocksWorld*)world);
	}

	void printInfo() {
		OUTPUT << "I'm a RobotBlocksWorld" << endl;
	}

	virtual RobotBlocksBlock* getBlockById(int bId) {
		return((RobotBlocksBlock*)World::getBlockById(bId));
	}

	virtual void addBlock(int blockId, RobotBlocksBlockCode *(*robotBlockCodeBuildingFunction)(RobotBlocksBlock*), const Cell3DPosition &pos, const Color &col, bool master=false);
	void deleteBlock(RobotBlocksBlock *bb);
	inline void setBlocksSize(float *siz) { blockSize[0] = siz[0]; blockSize[1] = siz[1]; blockSize[2] = siz[2]; };
	inline const float *getBlocksSize() { return blockSize; };

	inline void setGridPtr(int ix,int iy,int iz,RobotBlocksBlock *ptr) { gridPtrBlocks[ix+(iy+iz*gridSize[1])*gridSize[0]]=ptr; };
	inline RobotBlocksBlock* getGridPtr(int ix,int iy,int iz) { return gridPtrBlocks[ix+(iy+iz*gridSize[1])*gridSize[0]]; };

	inline presence *getTargetGridPtr(int *gs) { memcpy(gs,gridSize,3*sizeof(int)); return targetGrid; };
	inline presence getTargetGrid(int ix,int iy,int iz) { return targetGrid[(iz*gridSize[1]+iy)*gridSize[0]+ix]; };
	inline void setTargetGrid(presence value,int ix,int iy,int iz) { targetGrid[(iz*gridSize[1]+iy)*gridSize[0]+ix]=value; };
	void initTargetGrid();

	inline void setCapabilities(RobotBlocksCapabilities *capa) { capabilities=capa; };
	void getPresenceMatrix(const PointRel3D &pos,PresenceMatrix &pm);
	inline RobotBlocksCapabilities* getCapabilities() { return capabilities; };
	void linkBlocks();
	void loadTextures(const string &str);
	virtual void glDraw();
	virtual void glDrawId();
	virtual void glDrawIdByMaterial();
	void updateGlData(RobotBlocksBlock*blc,int prev,int next);
	virtual void createHelpWindow();
	inline virtual Camera *getCamera() { return camera; };
	virtual void setSelectedFace(int n);
	virtual void menuChoice(int n);
	virtual void disconnectBlock(RobotBlocksBlock *block);
	virtual void connectBlock(RobotBlocksBlock *block);
	virtual bool canAddBlockToFace(int numSelectedBlock, int numSelectedFace);
	virtual void exportConfiguration();
};

inline void createWorld(int slx,int sly,int slz, int argc, char *argv[]) {
	RobotBlocksWorld::createWorld(slx,sly,slz, argc,argv);
}

inline void deleteWorld() {
	RobotBlocksWorld::deleteWorld();
}

inline RobotBlocksWorld* getWorld() { return(RobotBlocksWorld::getWorld()); }

} // RobotBlocks namespace

#endif /* ROBOTBLOCKSWORLD_H_ */
