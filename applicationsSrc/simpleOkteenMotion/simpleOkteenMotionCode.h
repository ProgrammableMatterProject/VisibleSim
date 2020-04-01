#ifndef simpleMotionCode_H_
#define simpleMotionCode_H_

#include "robots/okteen/okteenSimulator.h"
#include "robots/okteen/okteenBlockCode.h"

using namespace Okteen;

class SimpleOkteenMotionCode : public OkteenBlockCode {
private:
    OkteenBlock *module;
    int step;
    // metamodule data
    // leader
    OkteenBlock** members;
    // path to leader
    P2PNetworkInterface *pathToLeader;
public:
    SimpleOkteenMotionCode(OkteenBlock *host):OkteenBlockCode(host) { module=host; };
    ~SimpleOkteenMotionCode() {};

    void startup() override;
    virtual void onMotionEnd(void) override;
    void sendMessageToMMLeader();

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return(new SimpleOkteenMotionCode((OkteenBlock*)host));
    };
/*****************************************************************************/
};


#endif /* simpleMotionCode_H_ */
