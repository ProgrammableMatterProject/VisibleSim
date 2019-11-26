#ifndef simpleReconfC2DCode_H_
#define simpleReconfC2DCode_H_

#include "catoms2DWorld.h"
#include "catoms2DBlockCode.h"

static const int GO_MSG_ID = 1001;

using namespace Catoms2D;

class SimpleReconfC2DCode : public Catoms2DBlockCode {
private:
    int layer;
    Catoms2DBlock *catom;
public :
    SimpleReconfC2DCode(Catoms2DBlock *host):Catoms2DBlockCode(host) {};
    ~SimpleReconfC2DCode() {};

    void startup() override;
    void processLocalEvent(EventPtr pev) override;
    void onMotionEnd();

    void handleGoMessage(MessagePtr anonMsg, P2PNetworkInterface *sender);

    void verticalLineToHorizontalLineStartup();
    void rotateCWAfterCheck();
    void sendGoMessageToNextModule(int msgLayer);


/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new SimpleReconfC2DCode((Catoms2DBlock*)host));
    };
/*****************************************************************************/
};

#endif /* simpleReconfC2DCode_H_ */
