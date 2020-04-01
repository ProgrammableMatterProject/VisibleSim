#ifndef datomsTestbedCode_H_
#define datomsTestbedCode_H_
#include "robots/datoms/datomsSimulator.h"
#include "robots/datoms/datomsBlockCode.h"
#include "robots/datoms/datomsMotionRules.h"

using namespace Datoms;

class DatomsTestbedCode : public DatomsBlockCode {
private:
    DatomsBlock *module;
    SkewFCCLattice *lattice;
    inline static unsigned short *tabDistances;
    inline static bool *tabLockedCells;
public :
    DatomsTestbedCode(DatomsBlock *host):DatomsBlockCode(host) { module = host; };
    ~DatomsTestbedCode() {};

    void setDistance(const Cell3DPosition &pos,unsigned short d);

    void initTabDistances();
    void startup() override;
    void onMotionEnd() override;
    void onTap(int) override {}
/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return(new DatomsTestbedCode((DatomsBlock*)host));
    };
/*****************************************************************************/
};

#endif /* datomsTestbedCode_H_ */
