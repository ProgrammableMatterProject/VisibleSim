#ifndef distanceRBCode_H_
#define distanceRBCode_H_
#include "robotBlocksGeneric.h"

static const int BROADCAST_MSG=1001;

using namespace RobotBlocks;

class DistanceMessage {
public:
	uint16_t distance;
	uint8_t level;

	DistanceMessage(uint16_t d,uint8_t l):distance(d),level(l) {};

};

class DistanceRBCode : public GenericCodeBlock {
private:
	uint16_t distance;
	uint8_t currentLevel;
public:
	DistanceRBCode(RobotBlocksBlock *host):GenericCodeBlock(host) {};
	~DistanceRBCode() {};

	void startup();
	void myBROADCAST_MSGFunc(const MessageOf<DistanceMessage>*msg,P2PNetworkInterface *sender);

	void onTap();

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static RobotBlocksBlockCode *buildNewBlockCode(RobotBlocksBlock *host) {
	    return(new DistanceRBCode(host));
	};
/*****************************************************************************/
};

void _myBROADCAST_MSGFunc(GenericCodeBlock *,MessagePtr,P2PNetworkInterface *sender);

#endif /* distanceRBCode_H_ */
