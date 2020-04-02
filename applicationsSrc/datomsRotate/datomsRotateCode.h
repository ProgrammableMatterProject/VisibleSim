#ifndef datomsRotateCode_H_
#define datomsRotateCode_H_
#include <queue>
#include "robots/datoms/datomsSimulator.h"
#include "robots/datoms/datomsBlockCode.h"
#include "robots/datoms/datomsMotionRules.h"
#include "motion/teleportationEvents.h"

using namespace Datoms;

class DatomsRotateCode : public DatomsBlockCode {
private:
    DatomsBlock *module;
    queue <Cell3DPosition> cellsList;
    SkewFCCLattice *lattice;
    bool distanceCalculated = false;
    inline static unsigned short *tabDistances = nullptr;
    inline static bool *tabLockedCells  = nullptr;
    inline static vector<Cell3DPosition> ptsLine;
    inline static bool showDistance = false;
public :
    DatomsRotateCode(DatomsBlock *host):DatomsBlockCode(host) {
        module = host;
    };

    ~DatomsRotateCode() {
        if (tabDistances) {
            delete [] tabDistances;
            tabDistances = nullptr;
        }

        if (tabLockedCells) {
            delete [] tabLockedCells;
            tabLockedCells = nullptr;
        }
    };

    void initDistances();
    void initTabDistances();
    void initTabLockedCells();
    unsigned short getDistance(const Cell3DPosition &pos);
    void setDistance(const Cell3DPosition &pos,unsigned short d);

    bool lockCell(const Cell3DPosition &pos);
    bool unlockCell(const Cell3DPosition &pos);

    void startup() override;
    bool tryToMove();
    void onMotionEnd() override;
    void onTap(int) override {}
    void onGlDraw() override;

    //void parseUserElements(TiXmlDocument *config);
/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return(new DatomsRotateCode((DatomsBlock*)host));
    };
/*****************************************************************************/
};

#endif /* datomsRotateCode_H_ */
