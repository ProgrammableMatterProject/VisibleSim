#ifndef DemoMotionsCode_H_
#define DemoMotionsCode_H_
#include <queue>
#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DBlockCode.h"
#include "robots/catoms3D/catoms3DMotionEngine.h"
#include "robots/catoms3D/catoms3DRotationEvents.h"
#include "motion/teleportationEvents.h"

using namespace Catoms3D;

class DemoMotionsCode : public Catoms3DBlockCode {
private:
    Catoms3DBlock *module;
    FCCLattice *lattice;
public :
    DemoMotionsCode(Catoms3DBlock *host):Catoms3DBlockCode(host) { module = host; };
    ~DemoMotionsCode() {};

    void startup() override;
    void initDistances();
    bool tryToMove();

    void onMotionEnd() override;
    /*****************************************************************************/
    /** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return(new DemoMotionsCode((Catoms3DBlock*)host));
    };
    /*****************************************************************************/
};

#endif /* DemoMotionsCode_H_ */
