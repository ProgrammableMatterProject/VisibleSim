#ifndef simpleColorCode_H_
#define simpleColorCode_H_
#include "robots/blinkyBlocks/blinkyBlocksBlockCode.h"

static const int BROADCAST_MSG=1001;

using namespace BlinkyBlocks;

class SimpleColorCode : public BlinkyBlocksBlockCode {
private:
    int distance;
public :
    SimpleColorCode(BlinkyBlocksBlock *host):BlinkyBlocksBlockCode(host) {};
    ~SimpleColorCode() {};

    void startup() override;
    void myBroadcastFunc(MessagePtr anonMsg, P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new SimpleColorCode((BlinkyBlocksBlock*)host));
    };
/*****************************************************************************/
};
#endif /* simpleColorCode_H_ */
