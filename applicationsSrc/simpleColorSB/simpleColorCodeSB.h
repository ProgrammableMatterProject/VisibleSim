#ifndef simpleColorCode_H_
#define simpleColorCode_H_
#include "robots/smartBlocks/smartBlocksBlockCode.h"

static const int BROADCAST_MSG=1001;

using namespace SmartBlocks;

class SimpleColorCode : public SmartBlocksBlockCode {
private:
    int distance;
public :
    SimpleColorCode(SmartBlocksBlock *host):SmartBlocksBlockCode(host) {};
    ~SimpleColorCode() {};

    void startup() override;
    void myBroadcastFunc(const std::shared_ptr<Message> msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new SimpleColorCode((SmartBlocksBlock*)host));
    };
/*****************************************************************************/
};
#endif /* simpleColorCode_H_ */
