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
#include "catoms2DDirection.h"
//#include "catoms2DMove.h"
#include <boost/asio.hpp>
#include <stdexcept>

namespace Catoms2D {

class Catoms2DBlockCode;
class Catoms2DMove;

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
        inline P2PNetworkInterface *getInterface(int d) {
            return tabInterfaces[(NeighborDirection::Direction)d];
        }
        P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const PointRel3D &pos);

	Vecteur getPosition(NeighborDirection::Direction d);
	Vecteur getPosition(P2PNetworkInterface *p2p);

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
        void startMove(Catoms2DMove &m, uint64_t t);
	void startMove(Catoms2DMove &m);
};

std::ostream& operator<<(std::ostream &stream, Catoms2DBlock const& bb);

}

#endif /* CATOMS2DBLOCK_H_ */
