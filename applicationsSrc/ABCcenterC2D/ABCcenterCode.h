#ifndef simpleColorCode_H_
#define simpleColorCode_H_
#include "catoms2DGeneric.h"

static const int SPINNING_TREE_MSG=1001;
static const int ACK_SP_MSG=1002;

enum ConnectorStates { NotConnected=0, IsNotChild, IsChild, StateNotKnown };
using namespace Catoms2D;

class ABCcenterCode : public GenericCodeBlock {
private:
    uint16_t distance;
    ConnectorStates tabChildren[nbreNeighborsMax];
    P2PNetworkInterface *parent;

    void initTabChildren();

public :
	ABCcenterCode(Catoms2DBlock *host):GenericCodeBlock(host) {};
	~ABCcenterCode() {};

	void startup();
	void mySpinningTreeFunc(const MessageOf<uint16_t>*msg,P2PNetworkInterface *sender);
	void myAckSPFunc(const MessageOf<uint16_t>*msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static Catoms2DBlockCode *buildNewBlockCode(Catoms2DBlock *host) {
	    return(new ABCcenterCode(host));
	};
/*****************************************************************************/
};
	void _mySpinningTreeFunc(GenericCodeBlock *,MessagePtr,P2PNetworkInterface *sender);
	void _myAckSPFunc(GenericCodeBlock *,MessagePtr,P2PNetworkInterface *sender);
#endif /* simpleColorCode_H_ */
