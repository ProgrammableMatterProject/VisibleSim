#ifndef simpleColorCode_H_
#define simpleColorCode_H_
#include "smartBlocksBlockCode.h"

static const int BROADCAST_MSG=1001;

using namespace SmartBlocks;

class SimpleColorCode : public SmartBlocksBlockCode {
private:
    int distance;
public :
	SimpleColorCode(SmartBlocksBlock *host):SmartBlocksBlockCode(host) {};
	~SimpleColorCode() {};

	void startup();
	void myBroadcastFunc(const MessageOf<int>*msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static SmartBlocksBlockCode *buildNewBlockCode(SmartBlocksBlock *host) {
	    return(new SimpleColorCode(host));
	};
/*****************************************************************************/
};
	void _myBroadcastFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);
#endif /* simpleColorCode_H_ */
