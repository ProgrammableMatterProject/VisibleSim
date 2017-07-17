#ifndef simpleColorCode_H_
#define simpleColorCode_H_
#include "okteenBlockCode.h"

static const int BROADCAST_MSG=1001;

using namespace Okteen;

class SimpleColorCode : public OkteenBlockCode {
private:
    int distance;
public :
	SimpleColorCode(OkteenBlock *host):OkteenBlockCode(host) {};
	~SimpleColorCode() {};

	void startup();
	void myBroadcastFunc(const MessageOf<int>*msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
	    return (new SimpleColorCode((OkteenBlock*)host));
	};
/*****************************************************************************/
};
	void _myBroadcastFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);
#endif /* simpleColorCode_H_ */
