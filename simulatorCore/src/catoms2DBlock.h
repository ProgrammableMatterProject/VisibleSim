/*
 * catoms2DBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS2DBLOCK_H_
#define CATOMS2DBLOCK_H_

#include <boost/asio.hpp>
#include <stdexcept>

#include "buildingBlock.h"
#include "catoms2DBlockCode.h"
#include "catoms2DGlBlock.h"
#include "catoms2DEvents.h"
#include "catoms2DDirection.h"
//#include "catoms2DMove.h"

namespace Catoms2D {

class Catoms2DBlockCode;
class Catoms2DMove;

class Catoms2DBlock : public BaseSimulator::BuildingBlock {
protected:

public:
	int angle;
	
	Catoms2DBlock(int bId, BlockCodeBuilder bcb);
	~Catoms2DBlock();

	inline void setGlBlock(Catoms2DGlBlock*ptr) { ptrGlBlock=ptr;};
	P2PNetworkInterface *getInterface(NeighborDirection::Direction d);
	inline P2PNetworkInterface *getInterface(int d) {
		return P2PNetworkInterfaces[(NeighborDirection::Direction)d];
	}
	P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const PointRel3D &pos);

	Cell3DPosition getPosition(NeighborDirection::Direction d);
	Cell3DPosition getPosition(P2PNetworkInterface *p2p);

	NeighborDirection::Direction getDirection(P2PNetworkInterface* p2p);
	int nbNeighbors(bool groundIsNeighbor = false);
	int nbConsecutiveNeighbors(bool groundIsNeighbor = false);
	int nbConsecutiveEmptyFaces(bool groundIsNeighbor = false);
	bool hasANeighbor(NeighborDirection::Direction n, bool groundIsNeighbor = false);
	bool hasANeighbor(P2PNetworkInterface *p2p, bool groundIsNeighbor = false);

	//inline direction_t getOpposite(direction_t d) { return (direction_t) (d * (-1));}  
	P2PNetworkInterface* getNextInterface(RelativeDirection::Direction dir,
										  P2PNetworkInterface *p2p, bool connected = false);

	// Motion
	bool isBlocked();
	bool canMove(Catoms2DMove &m);
	int getCCWMovePivotId();
	int getCWMovePivotId();
	void startMove(Catoms2DMove &m, uint64_t t);
	void startMove(Catoms2DMove &m);   
};

std::ostream& operator<<(std::ostream &stream, Catoms2DBlock const& bb);

}

#endif /* CATOMS2DBLOCK_H_ */
