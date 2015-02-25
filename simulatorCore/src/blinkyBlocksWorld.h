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
#include "vecteur.h"
#include "blinkyBlocksBlock.h"
#include "objLoader.h"
#include <boost/asio.hpp>
#include "trace.h"
#include <blinkyBlocksScenario.h>
#include <vector>

namespace BlinkyBlocks {

class BlinkyBlocksWorld : BaseSimulator::World {
protected:
	int gridSize[3];
	BlinkyBlocksBlock **gridPtrBlocks;
	GLfloat blockSize[3];
	Camera *camera;
	ObjLoader::ObjLoader *objBlock,*objBlockForPicking,*objRepere;
	GLuint idTextureWall;
	GLushort numSelectedFace;
	GLuint numSelectedBlock;
	GLint menuId;
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
	
	void printInfo() {
		OUTPUT << "I'm a BlinkyBlocksWorld" << endl;
	}

	virtual BlinkyBlocksBlock* getBlockById(int bId) {
		return((BlinkyBlocksBlock*)World::getBlockById(bId));
	}

	virtual void addBlock(int blockId, BlinkyBlocksBlockCode *(*blinkyBlockCodeBuildingFunction)(BlinkyBlocksBlock*), const Vecteur &pos, const Vecteur &col);
	void deleteBlock(BlinkyBlocksBlock *bb);
	inline void setBlocksSize(float *siz) { blockSize[0] = siz[0]; blockSize[1] = siz[1]; blockSize[2] = siz[2]; };

	void linkBlocks();
	void loadTextures(const string &str);
	virtual void glDraw();
	virtual void glDrawId();
	virtual void glDrawIdByMaterial();
	virtual void updateGlData(BlinkyBlocksBlock*blc);
	virtual void createPopupMenu(int ix, int iy);
	virtual void createHelpWindow();
	inline virtual Camera *getCamera() { return camera; };
	virtual void setSelectedFace(int n);
	virtual void menuChoice(int n);
	
	/* Sends the appropriate message (tap, ...) to the VM associated to bId block (through the scheduler)*/
	void tapBlock(uint64_t date, int bId);
	void accelBlock(uint64_t date, int bId, int x, int y, int z);
	void shakeBlock(uint64_t date, int bId, int f);	
	void stopBlock(uint64_t date, int bId);
	
	/* Sends directly (not scheduled) a message to all the active VMs of the world.
	 * Returns to how many nodes the message has been sent.
	 */
	int broadcastDebugCommand(DebbuggerVMCommand &c);
	int sendCommand(int id, VMCommand &c);
	
	bool dateHasBeenReachedByAll(uint64_t date);
	bool equilibrium();
	
	void killAllVMs();
	void addScenarioEvent(ScenarioEvent *ev) { tabEvents.push_back(ev); };
   void closeAllSockets();
   
   // Prints information about the blocks
   void dump();
   
};

inline void createWorld(int slx,int sly,int slz, int argc, char *argv[]) {
	BlinkyBlocksWorld::createWorld(slx,sly,slz, argc,argv);
}

inline void deleteWorld() {
	BlinkyBlocksWorld::deleteWorld();
}

inline BlinkyBlocksWorld* getWorld() { return(BlinkyBlocksWorld::getWorld()); }

} // BlinkyBlocks namespace

#endif /* BLINKYBLOCKSWORLD_H_ */
