/*
 * catoms2DBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS2DBLOCK_H_
#define CATOMS2DBLOCK_H_

#include "buildingBlock.h"
#include "catoms2DBlockCode.h"
#include "catoms2DGlBlock.h"
#include "catoms2DCapabilities.h"
#include "catoms2DEvents.h"
#include <boost/asio.hpp>
#include <stdexcept>

namespace Catoms2D {

#define MAX_NB_NEIGHBORS 6

class NeighborDirection {
public:
	enum Direction { Right = 0, TopRight = 1, TopLeft = 2, Left = 3, BottomLeft = 4, BottomRight = 5};
	static int getOpposite(int d);
	static string getString(int d);
};

class Catoms2DBlockCode;

class Catoms2DBlock : public BaseSimulator::BuildingBlock {
	P2PNetworkInterface *tabInterfaces[6];
protected:
	boost::interprocess::interprocess_mutex mutex_vm;

public:
	Catoms2DGlBlock *ptrGlBlock;
	Color color; // color of the block
	Vecteur position; // position of the block;
	bool isMaster;
	int angle;

	Catoms2DBlockCode *(*buildNewBlockCode)(Catoms2DBlock*);
	Catoms2DBlock(int bId, Catoms2DBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)(Catoms2DBlock*));
	~Catoms2DBlock();

	inline Catoms2DGlBlock* getGlBlock() { return ptrGlBlock; };
	inline void setGlBlock(Catoms2DGlBlock*ptr) { ptrGlBlock=ptr;};
	void setColor(const Color &);
	void setPosition(const Vecteur &p);
	P2PNetworkInterface *getInterface(NeighborDirection::Direction d);
	
	NeighborDirection::Direction getDirection(P2PNetworkInterface*);
	int nbNeighbors();
	int nbConsecutiveNeighbors();

	// Motion
	bool canMove(Catoms2DBlock *pivot, direction_t direction);
        void startMove(Catoms2DBlock *pivot, direction_t direction, uint64_t t);
	void startMove(Catoms2DBlock *pivot, direction_t direction);
};

std::ostream& operator<<(std::ostream &stream, Catoms2DBlock const& bb);

}

#endif /* CATOMS2DBLOCK_H_ */
