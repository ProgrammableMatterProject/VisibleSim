/*
 * catoms2DWorld.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS2DWORLD_H_
#define CATOMS2DWORLD_H_

#include "openglViewer.h"
#include "world.h"
#include "vecteur.h"
#include "catoms2DBlock.h"
#include "catoms2DCapabilities.h"
#include "objLoader.h"
#include <boost/asio.hpp>
#include "trace.h"
#include <vector>

namespace Catoms2D {

class Catoms2DWorld : public BaseSimulator::World {
protected:
	int gridSize[3];
	Catoms2DBlock **gridPtrBlocks;
	GLfloat blockSize[3];
	Camera *camera;
	ObjLoader::ObjLoader *objBlock,*objBlockForPicking,*objRepere;
	GLuint idTextureHexa,idTextureLines;
	presence *targetGrid;
        Catoms2DCapabilities *capabilities;
        static const int numPickingTextures = 7; /* The number of picking textures defined for 
                                                         this type of catom, 
                                                         used to deduce selected Block / face */

	Catoms2DWorld(int slx,int sly,int slz, int argc, char *argv[]);
	virtual ~Catoms2DWorld();
	void linkBlock(int ix,int iy,int iz);

public:
	static void createWorld(int slx,int sly,int slz, int argc, char *argv[]);
	static void deleteWorld();
	static Catoms2DWorld* getWorld() {
		assert(world != NULL);
		return((Catoms2DWorld*)world);
	}

	void printInfo() {
		OUTPUT << "I'm a Catoms2DWorld" << endl;
	}

	virtual Catoms2DBlock* getBlockById(int bId) {
		return((Catoms2DBlock*)World::getBlockById(bId));
	}

	virtual void addBlock(int blockId, Catoms2DBlockCode *(*robotBlockCodeBuildingFunction)(Catoms2DBlock*), const Vecteur &pos, const Color &col, bool master=false);
	void deleteBlock(Catoms2DBlock *bb);
	inline void setBlocksSize(float *siz) { blockSize[0] = siz[0]; blockSize[1] = siz[1]; blockSize[2] = siz[2]; };
	inline const float *getBlocksSize() { return blockSize; };

	inline void setGridPtr(int ix,int iy,int iz,Catoms2DBlock *ptr) { gridPtrBlocks[ix+(iy+iz*gridSize[1])*gridSize[0]]=ptr; };
	inline Catoms2DBlock* getGridPtr(int ix,int iy,int iz) { return gridPtrBlocks[ix+(iy+iz*gridSize[1])*gridSize[0]]; };
	inline int* getGridSize() {return gridSize;}
	inline presence *getTargetGridPtr(int *gs) { memcpy(gs,gridSize,3*sizeof(int)); return targetGrid; };
	inline presence getTargetGrid(int ix,int iy,int iz) { return targetGrid[(iz*gridSize[1]+iy)*gridSize[0]+ix]; };
	inline void setTargetGrid(presence value,int ix,int iy,int iz) { targetGrid[(iz*gridSize[1]+iy)*gridSize[0]+ix]=value; };
	void initTargetGrid();

	inline void setCapabilities(Catoms2DCapabilities *capa) { capabilities=capa; };
	void getPresenceMatrix(const PointRel3D &pos,PresenceMatrix &pm);
	inline Catoms2DCapabilities* getCapabilities() { return capabilities; };
	void linkBlocks();
	void loadTextures(const string &str);

	Vecteur worldToGridPosition(Vecteur &pos);
	Vecteur gridToWorldPosition(Vecteur &pos);

	bool areNeighborsWorldPos(Vecteur &pos1, Vecteur &pos2);
	bool areNeighborsGridPos(Vecteur &pos1, Vecteur &pos2);
	
	virtual void glDraw();
	virtual void glDrawId();
	virtual void glDrawIdByMaterial();
	virtual void updateGlData(Catoms2DBlock*blc);
	virtual void updateGlData(Catoms2DBlock*blc, const Vecteur &position);
	virtual void updateGlData(Catoms2DBlock*blc, const Vecteur &position, double angle);
	virtual void createHelpWindow();
	virtual void createPopupMenu(int ix,int iy);
	inline virtual Camera *getCamera() { return camera; };
	virtual void setSelectedFace(int n);
	virtual void menuChoice(int n);
	virtual void disconnectBlock(Catoms2DBlock *block);
	virtual void connectBlock(Catoms2DBlock *block);
        virtual bool canAddBlockToFace(int numSelectedBlock, int numSelectedFace);

	virtual void exportConfiguration();
};

inline void createWorld(int slx,int sly,int slz, int argc, char *argv[]) {
	Catoms2DWorld::createWorld(slx,sly,slz, argc,argv);
}

inline void deleteWorld() {
	Catoms2DWorld::deleteWorld();
}

inline Catoms2DWorld* getWorld() { return(Catoms2DWorld::getWorld()); }

} // Catoms2D namespace

#endif /* CATOMS2DWORLD_H_ */
