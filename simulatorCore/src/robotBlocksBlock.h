/*
 * robotBlocksBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef ROBOTBLOCKSBLOCK_H_
#define ROBOTBLOCKSBLOCK_H_

#include "buildingBlock.h"
#include "robotBlocksBlockCode.h"
#include "robotBlocksGlBlock.h"
#include "robotBlocksCapabilities.h"
#include <boost/asio.hpp>
#include <stdexcept>

namespace RobotBlocks {

class NeighborDirection {
public:
	enum Direction { Bottom = 0, Back = 1, Right = 2, Front = 3, Left = 4, Top =5};
	static int getOpposite(int d);
	static string getString(int d);
};

class RobotBlocksBlockCode;

class RobotBlocksBlock : public BaseSimulator::BuildingBlock {
	P2PNetworkInterface *tabInterfaces[6];
protected:
	boost::interprocess::interprocess_mutex mutex_vm;
public:
	RobotBlocksGlBlock *ptrGlBlock;

	RobotBlocksBlockCode *(*buildNewBlockCode)(RobotBlocksBlock*);
	RobotBlocksBlock(int bId, RobotBlocksBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)(RobotBlocksBlock*));
	~RobotBlocksBlock();

	inline RobotBlocksGlBlock* getGlBlock() { return ptrGlBlock; };
	inline void setGlBlock(RobotBlocksGlBlock*ptr) { ptrGlBlock=ptr;};
	void setPrevNext(int,int);
	void setPrevNext(const P2PNetworkInterface *prev,const P2PNetworkInterface *next);
	P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const PointRel3D &pos);
	inline P2PNetworkInterface *getInterface(NeighborDirection::Direction d) { return tabInterfaces[d]; }

	NeighborDirection::Direction getDirection(P2PNetworkInterface*);
};

std::ostream& operator<<(std::ostream &stream, RobotBlocksBlock const& bb);

}

#endif /* ROBOTBLOCKSBLOCK_H_ */
