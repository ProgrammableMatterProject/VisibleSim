#ifndef distanceRBCode_H_
#define distanceRBCode_H_
#include "robotBlocksBlockCode.h"

static const int BROADCAST_MSG=1001;

using namespace RobotBlocks;

class DistanceRBCode : public RobotBlocksBlockCode {
private:
	uint16_t distance;
public:
	DistanceRBCode(RobotBlocksBlock *host):RobotBlocksBlockCode(host) {};
	~DistanceRBCode() {};

	void startup();
	void myBROADCAST_MSGFunc(const MessageOf<uint16_t>*msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
	    return (new DistanceRBCode((RobotBlocksBlock*)host));
	};
/*****************************************************************************/
};

void _myBROADCAST_MSGFunc(BlockCode*,MessagePtr,P2PNetworkInterface *sender);

#endif /* distanceRBCode_H_ */
