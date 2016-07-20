#ifndef distanceRBCode_H_
#define distanceRBCode_H_
#include "robotBlocksBlockCode.h"
#include "network.h"

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
	static RobotBlocksBlockCode *buildNewBlockCode(RobotBlocksBlock *host) {
	    return(new DistanceRBCode(host));
	};
/*****************************************************************************/
};

void _myBROADCAST_MSGFunc(BlockCode*,MessagePtr,P2PNetworkInterface *sender);

#endif /* distanceRBCode_H_ */
