#ifndef simpleColorCode_H_
#define simpleColorCode_H_

#include "robots/slidingCubes/slidingCubesBlockCode.h"

static const int BROADCAST_MSG=1001;

using namespace SlidingCubes;

class SimpleColorCode : public SlidingCubesBlockCode {
private:
    int distance;
public :
    SimpleColorCode(SlidingCubesBlock *host):SlidingCubesBlockCode(host) {};
    ~SimpleColorCode() {};

    void startup() override;
    void myBroadcastFunc(std::shared_ptr<Message> msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new SimpleColorCode((SlidingCubesBlock*)host));
    };
/*****************************************************************************/
};
#endif /* simpleColorCode_H_ */
