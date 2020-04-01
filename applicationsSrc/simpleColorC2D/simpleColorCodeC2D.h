#ifndef simpleColorCode_H_
#define simpleColorCode_H_
#include "robots/catoms2D/catoms2DBlockCode.h"

static const int BROADCAST_MSG=1001;

using namespace Catoms2D;

class SimpleColorCode : public Catoms2DBlockCode {
private:
    int distance;
public :
    SimpleColorCode(Catoms2DBlock *host):Catoms2DBlockCode(host) {};
    ~SimpleColorCode() {};

    void startup() override;
    void myBroadcastFunc(MessagePtr msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new SimpleColorCode((Catoms2DBlock*)host));
    };
/*****************************************************************************/
};
    void _myBroadcastFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);
#endif /* simpleColorCode_H_ */
