#ifndef simpleColorCode_H_
#define simpleColorCode_H_
#include "blinkyBlocksGeneric.h"

static const int BROADCAST_MSG=1001;

using namespace BlinkyBlocks;

class SimpleColorCode : public GenericCodeBlock {
private:
    int distance;
public :
	SimpleColorCode(BlinkyBlocksBlock *host):GenericCodeBlock(host) {};
	~SimpleColorCode() {};

	void startup();
	void myBroadcastFunc(const MessageOf<int>*msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlinkyBlocksBlockCode *buildNewBlockCode(BlinkyBlocksBlock *host) {
	    return(new SimpleColorCode(host));
	};
/*****************************************************************************/
};
	void _myBroadcastFunc(GenericCodeBlock *,MessagePtr,P2PNetworkInterface *sender);
#endif /* simpleColorCode_H_ */
