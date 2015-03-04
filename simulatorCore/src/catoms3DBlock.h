/*
 * catoms3DBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS3DBLOCK_H_
#define CATOMS3DBLOCK_H_

#include "buildingBlock.h"
#include "catoms3DBlockCode.h"
#include "catoms3DGlBlock.h"
#include "catoms3DCapabilities.h"
#include <boost/asio.hpp>
#include <stdexcept>

namespace Catoms3D {

class NeighborDirection {
public:
	enum Direction { BottomLeft = 0, Left = 1, TopLeft = 2, BottomRight = 3, Right = 4, TopRight =5};
	static int getOpposite(int d);
	static string getString(int d);
};

class Catoms3DBlockCode;

class Catoms3DBlock : public BaseSimulator::BuildingBlock {
	P2PNetworkInterface *tabInterfaces[6];
protected:
	boost::interprocess::interprocess_mutex mutex_vm;

public:
	Catoms3DGlBlock *ptrGlBlock;
	Color color; // color of the block
	Vecteur position; // position of the block;
	bool isMaster;

	Catoms3DBlockCode *(*buildNewBlockCode)(Catoms3DBlock*);
	Catoms3DBlock(int bId, Catoms3DBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)(Catoms3DBlock*));
	~Catoms3DBlock();

	inline Catoms3DGlBlock* getGlBlock() { return ptrGlBlock; };
	inline void setGlBlock(Catoms3DGlBlock*ptr) { ptrGlBlock=ptr;};
	void setColor(const Color &);
	void setPosition(const Vecteur &p);
	P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const PointRel3D &pos);
	inline P2PNetworkInterface *getInterface(NeighborDirection::Direction d) { return tabInterfaces[d]; }

	NeighborDirection::Direction getDirection(P2PNetworkInterface*);
};

std::ostream& operator<<(std::ostream &stream, Catoms3DBlock const& bb);

}

#endif /* CATOMS3DBLOCK_H_ */
