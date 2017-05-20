#ifndef simpleColorCode_H_
#define simpleColorCode_H_
#include "oktenBlockCode.h"

static const int BROADCAST_MSG=1001;

using namespace Okten;

class SimpleColorCode : public OktenBlockCode {
private:
    int distance;
public :
	SimpleColorCode(OktenBlock *host):OktenBlockCode(host) {};
	~SimpleColorCode() {};

	void startup();
	void myBroadcastFunc(const MessageOf<int>*msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
	    return (new SimpleColorCode((OktenBlock*)host));
	};
/*****************************************************************************/
};
	void _myBroadcastFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);
#endif /* simpleColorCode_H_ */
