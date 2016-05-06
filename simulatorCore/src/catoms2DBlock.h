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
#include <boost/asio.hpp>
#include <stdexcept>

namespace Catoms2D {

class NeighborDirection {
public:
	enum Direction { BottomLeft = 0, Left = 1, TopLeft = 2, BottomRight = 3, Right = 4, TopRight =5};
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

	Catoms2DBlockCode *(*buildNewBlockCode)(Catoms2DBlock*);
	Catoms2DBlock(int bId, Catoms2DBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)(Catoms2DBlock*));
	~Catoms2DBlock();

	inline Catoms2DGlBlock* getGlBlock() { return ptrGlBlock; };
	inline void setGlBlock(Catoms2DGlBlock*ptr) { ptrGlBlock=ptr;};
	void setColor(const Color &);
	void setPosition(const Vecteur &p);
	P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const PointRel3D &pos);
	inline P2PNetworkInterface *getInterface(NeighborDirection::Direction d) { return tabInterfaces[d]; }
	inline P2PNetworkInterface *getInterface(int d) { return tabInterfaces[(NeighborDirection::Direction)d]; }
	NeighborDirection::Direction getDirection(P2PNetworkInterface*);
};

std::ostream& operator<<(std::ostream &stream, Catoms2DBlock const& bb);

}

#endif /* CATOMS2DBLOCK_H_ */
