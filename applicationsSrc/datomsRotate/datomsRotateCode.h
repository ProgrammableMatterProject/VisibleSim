#ifndef datomsRotateCode_H_
#define datomsRotateCode_H_
#include "datomsSimulator.h"
#include "datomsBlockCode.h"
#include "datomsMotionRules.h"

static const int LOCK_MSG=1001;
static const int ANSLOCK_MSG=1002;
static const int UNLOCK_MSG=1003;

using namespace Datoms;

class Motions {
public :
    vector <Cell3DPosition> tabCells;
    DatomsBlock *mobile,*fixed;
    DatomsMotionRulesLink *MRlist;

    Motions(DatomsBlock *m,DatomsBlock *f,DatomsMotionRulesLink *mrl);
    ~Motions();
};


class DatomsRotateCode : public DatomsBlockCode {
private:
    DatomsBlock *module;
    bool isLocked;
    Motions *currentMotion;
    SkewFCCLattice *lattice;
    inline static unsigned short *tabDistances;
    inline static bool *tabLockedCells;
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
    void myLockFunc(const MessageOf<Motions>*msg,P2PNetworkInterface *sender);
    void myAnsLockFunc(const MessageOf<bool>*msg,P2PNetworkInterface *sender);
    void myUnlockFunc(P2PNetworkInterface *sender);
    void onMotionEnd() override;
    void onTap(int) override;
    void onGlDraw() override;

    //void parseUserElements(TiXmlDocument *config);
/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return(new DatomsRotateCode((DatomsBlock*)host));
    };
/*****************************************************************************/
};

void _myLockFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);
void _myAnsLockFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);
void _myUnlockFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);

#endif /* datomsRotateCode_H_ */
