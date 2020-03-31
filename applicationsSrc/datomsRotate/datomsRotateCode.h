#ifndef datomsRotateCode_H_
#define datomsRotateCode_H_
#include <queue>
#include "datomsSimulator.h"
#include "datomsBlockCode.h"
#include "datomsMotionRules.h"
#include "teleportationEvents.h"

using namespace Datoms;

class DatomsRotateCode : public DatomsBlockCode {
private:
    DatomsBlock *module;
    queue <Cell3DPosition> cellsList;
    SkewFCCLattice *lattice;
    bool distanceCalculated = false;
    inline static unsigned short *tabDistances;
    inline static bool *tabLockedCells;
    inline static vector<Cell3DPosition> ptsLine;
    inline static bool showDistance = false;
public :
    DatomsRotateCode(DatomsBlock *host):DatomsBlockCode(host) {
        module = host;
        tabDistances = nullptr;
        tabLockedCells = new bool[lattice->gridSize[0]
                                  * lattice->gridSize[1] * lattice->gridSize[2]]();
    };

    ~DatomsRotateCode() {
        delete [] tabDistances;
        delete [] tabLockedCells;
    };

    void initDistances();
    void initTabDistances();
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
