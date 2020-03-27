#ifndef simpleColorCode_H_
#define simpleColorCode_H_
#include "robotBlocksBlockCode.h"

static const int BROADCAST_MSG=1001;

using namespace RobotBlocks;

class SimpleColorCode : public RobotBlocksBlockCode {
private:
    int distance;
public :
    SimpleColorCode(RobotBlocksBlock *host):RobotBlocksBlockCode(host) {};
    ~SimpleColorCode() {};

    void startup() override;
    void myBroadcastFunc(const MessageOf<int>*msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new SimpleColorCode((RobotBlocksBlock*)host));
    };
/*****************************************************************************/
};
    void _myBroadcastFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);
#endif /* simpleColorCode_H_ */
