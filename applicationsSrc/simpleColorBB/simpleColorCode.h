#ifndef simpleColorCode_H_
#define simpleColorCode_H_
#include "blinkyBlocksBlockCode.h"

static const int BROADCAST_MSG=1001;

using namespace BlinkyBlocks;

class SimpleColorCode : public BlinkyBlocksBlockCode {
private:
    int distance;
public :
    SimpleColorCode(BlinkyBlocksBlock *host):BlinkyBlocksBlockCode(host) {};
    ~SimpleColorCode() {};

    void startup();
    void myBroadcastFunc(const MessageOf<int>*msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new SimpleColorCode((BlinkyBlocksBlock*)host));
    };
/*****************************************************************************/
};
    void _myBroadcastFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);
#endif /* simpleColorCode_H_ */
