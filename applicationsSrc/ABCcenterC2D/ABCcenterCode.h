#ifndef simpleColorCode_H_
#define simpleColorCode_H_
#include "catoms2DGeneric.h"

static const int SPINNING_TREE_MSG=1001;
static const int ACK_SP_MSG=1002;
static const int GO2B_MSG=1003;

enum ConnectorStates { NotConnected=0, IsNotChild, IsChild, StateNotKnown };
using namespace Catoms2D;

class ackData {
    public :
        uint16_t distance; /// distance of the sender
        uint16_t distanceMax; /// distance max of the branch
        bool toParent; /// if sent to parent
    ackData(bool tp,uint16_t dst,uint16_t dstMax):toParent(tp),distance(dst),distanceMax(dstMax) {};
};

class ABCcenterCode : public GenericCodeBlock {
private:
    uint16_t moduleDistance; /// distance of the module
    uint16_t currentDistanceMax; /// distance max of the elected branch
    uint8_t electedChild; /// child branch with max distance
    ConnectorStates tabChildren[nbreNeighborsMax];
    P2PNetworkInterface *parent; /// parent of the module

    void initModuleData();

public :
	ABCcenterCode(Catoms2DBlock *host):GenericCodeBlock(host) {};
	~ABCcenterCode() {};

	void startup();
	void mySpinningTreeFunc(const MessageOf<uint16_t>*msg,P2PNetworkInterface *sender);
	void myAckSPFunc(const MessageOf<ackData>*msg,P2PNetworkInterface *sender);
	void myGo2BFunc(const MessageOf<uint8_t>*msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static Catoms2DBlockCode *buildNewBlockCode(Catoms2DBlock *host) {
	    return(new ABCcenterCode(host));
	};
/*****************************************************************************/
};
	void _mySpinningTreeFunc(GenericCodeBlock *,MessagePtr,P2PNetworkInterface *sender);
	void _myAckSPFunc(GenericCodeBlock *,MessagePtr,P2PNetworkInterface *sender);
	void _myGo2BFunc(GenericCodeBlock *,MessagePtr,P2PNetworkInterface *sender);
#endif /* simpleColorCode_H_ */
