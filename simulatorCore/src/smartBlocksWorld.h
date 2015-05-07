/*
 * smartBlocksWorld.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef SMARTBLOCKSWORLD_H_
#define SMARTBLOCKSWORLD_H_

#include "openglViewer.h"
#include "world.h"
#include "vecteur.h"
#include "smartBlocksBlock.h"
#include "smartBlocksCapabilities.h"
#include "objLoader.h"

namespace SmartBlocks {

class SmartBlocksWorld : BaseSimulator::World {
protected:
	SmartBlocksBlock **tabPtrBlocks;
	int gridSize[2];
	GLfloat blockSize[3];
	Camera *camera;
	ObjLoader::ObjLoader *objBlock,*objRepere;
	presence *targetGrid;
	SmartBlocksCapabilities *capabilities;
    int tabStatsData[10];
    int nbreStats;

	SmartBlocksWorld(int gw,int gh,int argc, char *argv[]);
	virtual ~SmartBlocksWorld();

	void linkBlock(int ix,int iy);
public:
	GLuint idTextureFloor,idTextureDigits;

	static void createWorld(int gw,int gh,int argc, char *argv[]);
	static void deleteWorld();
	static SmartBlocksWorld* getWorld() {
		assert(world != NULL);
		return((SmartBlocksWorld*)world);
	}

	void printInfo() {
		cout << "I'm a SmartBlocksWorld" << endl;
	}

	virtual void addBlock(int blockId, SmartBlocksBlockCode *(*smartBlockCodeBuildingFunction)(SmartBlocksBlock*),const Vecteur &pos,const Color &col);
	inline void setBlocksSize(float *siz) { blockSize[0]=siz[0];blockSize[1]=siz[1];blockSize[2]=siz[2];};

	inline presence *getTargetGridPtr(int *gs) { memcpy(gs,gridSize,2*sizeof(int)); return targetGrid; };
	inline presence getTargetGrid(int ix,int iy) { return targetGrid[iy*gridSize[0]+ix]; };
	inline void setTargetGrid(presence value,int ix,int iy) { targetGrid[iy*gridSize[0]+ix]=value; };
	void initTargetGrid();

	inline SmartBlocksBlock* getGridPtr(int x,int y) { return tabPtrBlocks[x+y*gridSize[0]]; };
	inline void setGridPtr(int x,int y,SmartBlocksBlock *ptr) { tabPtrBlocks[x+y*gridSize[0]]=ptr; };

	inline void setCapabilities(SmartBlocksCapabilities *capa) { capabilities=capa; };
	void getPresenceMatrix0(const PointCel &pos,PresenceMatrix &pm);
	void getPresenceMatrix(const PointCel &pos,PresenceMatrix &pm);
	inline SmartBlocksCapabilities* getCapabilities() { return capabilities; };

	void linkBlocks();
	void loadTextures(const string &str);
	virtual void glDraw();
	virtual void glDrawId();
	virtual void updateGlData(SmartBlocksBlock*blc);
	inline virtual Camera *getCamera() { return camera; };

	virtual void disconnectBlock(SmartBlocksBlock *block);
	virtual void connectBlock(SmartBlocksBlock *block);
	inline void getGridSize(int &lx,int &ly) { lx = gridSize[0]; ly = gridSize[1]; }

	bool isBorder(int x,int y);
	int nbreWellPlacedBlock();
	void createStats(int);
	void addStat(int n,int v);
	void printStats();
};

inline void createWorld(int gw,int gh,int argc, char *argv[]) {
	SmartBlocksWorld::createWorld(gw,gh,argc,argv);
}

inline void deleteWorld() {
	SmartBlocksWorld::deleteWorld();
}

inline SmartBlocksWorld* getWorld() { return(SmartBlocksWorld::getWorld()); }

} // SmartBlocks namespace

#endif /* SMARTBLOCKSWORLD_H_ */
