/*
 * catoms2DBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS2DBLOCK_H_
#define CATOMS2DBLOCK_H_

#include <stdexcept>

#include "buildingBlock.h"
#include "catoms2DBlockCode.h"
#include "catoms2DGlBlock.h"
#include "lattice.h"

class Rotation2DMove;

namespace Catoms2D {

class Catoms2DBlockCode;

class RelativeDirection {
 public:
  enum Direction {CW = -1, CCW =1};
  static inline Direction getOpposite(Direction d) { return (Direction) ((-1)*d); }
};

class Catoms2DBlock : public BaseSimulator::BuildingBlock {
protected:

public:
	int angle;
	
	Catoms2DBlock(int bId, BlockCodeBuilder bcb);
	~Catoms2DBlock();

	inline void setGlBlock(Catoms2DGlBlock*ptr) { ptrGlBlock=ptr;};
	P2PNetworkInterface *getInterface(HLattice::Direction d);
	inline P2PNetworkInterface *getInterface(int d) {
		return P2PNetworkInterfaces[(HLattice::Direction)d];
	}

	Cell3DPosition getPosition(HLattice::Direction d);
	Cell3DPosition getPosition(P2PNetworkInterface *p2p);

	int getDirection(P2PNetworkInterface* p2p);
	int nbNeighbors(bool groundIsNeighbor = false);
	int nbConsecutiveNeighbors(bool groundIsNeighbor = false);
	int nbConsecutiveEmptyFaces(bool groundIsNeighbor = false);
	bool hasANeighbor(HLattice::Direction n, bool groundIsNeighbor = false);
	bool hasANeighbor(P2PNetworkInterface *p2p, bool groundIsNeighbor = false);

	//inline direction_t getOpposite(direction_t d) { return (direction_t) (d * (-1));}  
	P2PNetworkInterface* getNextInterface(RelativeDirection::Direction dir,
										  P2PNetworkInterface *p2p, bool connected = false);

	// Motion
	bool isBlocked();
	bool canMove(Rotation2DMove &m);
	int getCCWMovePivotId();
	int getCWMovePivotId();
	void startMove(Rotation2DMove &m, Time t);
	void startMove(Rotation2DMove &m);

	// MeldInterpreter
	/**
	 * @copydoc BuildingBlock::addNeighbor
	 */
	virtual void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target);
	/**
	 * @copydoc BuildingBlock::removeNeighbor
	 */
	virtual void removeNeighbor(P2PNetworkInterface *ni);

};

std::ostream& operator<<(std::ostream &stream, Catoms2DBlock const& bb);

}

#endif /* CATOMS2DBLOCK_H_ */
