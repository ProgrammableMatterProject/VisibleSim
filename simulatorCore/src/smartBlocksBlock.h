/*
 * smartBlocksBlock.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef SMARTBLOCKSBLOCK_H_
#define SMARTBLOCKSBLOCK_H_

#include "buildingBlock.h"
#include "smartBlocksBlockCode.h"
#include "smartBlocksGlBlock.h"
#include "smartBlocksCapabilities.h"
#include "network.h"

namespace SmartBlocks {
enum NeighborDirection { North=0, East, South, West};

class SmartBlocksBlockCode;

class SmartBlocksBlock : public BaseSimulator::BuildingBlock {
  P2PNetworkInterface *tabInterfaces[4];
  SmartBlocksGlBlock *ptrGlBlock;
public:
	Color color; // color of the block
	Vecteur position; // position of the block;
	bool wellPlaced,_isBorder;
	SmartBlocksBlockCode *(*buildNewBlockCode)(SmartBlocksBlock*);

	SmartBlocksBlock(int bId, SmartBlocksBlockCode *(*SmartBlocksBlockCodeBuildingFunction)(SmartBlocksBlock *));
	~SmartBlocksBlock();

	inline SmartBlocksGlBlock* getGlBlock() { return ptrGlBlock; };
	inline void setGlBlock(SmartBlocksGlBlock*ptr) { ptrGlBlock=ptr;};
	void setColor(const Color &);
	void setColor(int num);
	void setPosition(const Vecteur &p);
	inline void setDisplayedValue(int n) { ptrGlBlock->setDisplayedValue(n); };
	inline P2PNetworkInterface *getInterface(NeighborDirection d) { return tabInterfaces[d]; }
	P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const PointCel &pos);
	P2PNetworkInterface *getP2PNetworkInterfaceByDestBlockId(int id);

	NeighborDirection getDirection( P2PNetworkInterface*);
	inline void getGridPosition(int &x,int &y) { x = int(position[0]); y=int(position[1]); };
};

}

#endif /* SMARTBLOCKSBLOCK_H_ */

