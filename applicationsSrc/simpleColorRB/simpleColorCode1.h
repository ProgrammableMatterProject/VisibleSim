#ifndef simpleColorCode_H_
#define simpleColorCode_H_
#include "robotBlocksGeneric.h"

static const int BROADCAST_MSG=1001;
static const int ACK_MSG=1002;

using namespace RobotBlocks;

class SimpleColorCode : public RobotBlocksGeneric {
private:
    int distance;
    int nbWaitedAnswers;
    P2PNetworkInterface *parent;
public :
	SimpleColorCode(RobotBlocksBlock *host):RobotBlocksGeneric(host) {};
	~SimpleColorCode() {};

	void startup();
	void myBroadcastFunc(const MessageOf<int>*msg,P2PNetworkInterface *sender);
	void myAckFunc(const MessageOf<int>*msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static RobotBlocksBlockCode *buildNewBlockCode(RobotBlocksBlock *host) {
	    return(new SimpleColorCode(host));
	};
/*****************************************************************************/
};
	void _myBroadcastFunc(RobotBlocksGeneric *,MessagePtr,P2PNetworkInterface *sender);
	void _myAckFunc(RobotBlocksGeneric *,MessagePtr,P2PNetworkInterface *sender);
#endif /* simpleColorCode_H_ */
