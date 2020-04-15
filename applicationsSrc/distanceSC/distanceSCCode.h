#ifndef distanceSCCode_H_
#define distanceSCCode_H_
#include "robots/slidingCubes/slidingCubesBlockCode.h"

static const int BROADCAST_MSG=1001;

using namespace SlidingCubes;

class DistanceSCCode : public SlidingCubesBlockCode {
private:
    uint16_t distance;
public:
    DistanceSCCode(SlidingCubesBlock *host):SlidingCubesBlockCode(host) {};
    ~DistanceSCCode() {};

    void startup() override;
    void myBROADCAST_MSGFunc(const std::shared_ptr<Message> msg,
                             P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new DistanceSCCode((SlidingCubesBlock*)host));
    };
/*****************************************************************************/
};

#endif /* distanceSCCode_H_ */
