#include <queue>
#include <climits>
#include "datomsTestbedCode.h"
#include "robots/datoms/datomsMotionEngine.h"

void DatomsTestbedCode::initTabDistances() {
    if (tabDistances == NULL) {
        const Cell3DPosition& gs = lattice->gridSize;
        int n = gs.pt[0]*gs.pt[1]*gs.pt[2];
        tabDistances = new unsigned short[n];
        // initialisation of tabDistances with value 'd'
        unsigned short *ptr = tabDistances;
        while (n--) {
            *ptr++=USHRT_MAX;
        }
    }
}

void DatomsTestbedCode::setDistance(const Cell3DPosition &pos,unsigned short d) {
    if (lattice->isInGrid(pos)) tabDistances[lattice->getIndex(pos)]=d;
}

void DatomsTestbedCode::startup() {
    lattice = (SkewFCCLattice*)(Datoms::getWorld()->lattice);

    if (module->blockId==1) { // mobile
        initTabDistances();

        // find list of motions
        DatomsMotionRules *motionRules = DatomsWorld::getWorld()->getMotionRules();
        // find pivot in neighbor list
        vector<BuildingBlock*> neighborBlock = module->getNeighbors();
        DatomsBlock *pivot=NULL;
        for (BuildingBlock* bb:neighborBlock) {
            if (bb->blockId==2) pivot=(DatomsBlock*)bb;
        }
      assert(pivot!=NULL);
        short j=pivot->getConnectorId(module->position);
        vector<DatomsMotionRulesLink*>tabMRL;
        cout << "pivot " << pivot->blockId << "#" << j << endl;
        if (motionRules->getValidMotionList(pivot, j, tabMRL)) { // list of theoretical motion
            for (DatomsMotionRulesLink* mrl:tabMRL) {
                Cell3DPosition pDest;
                pivot->getNeighborPos(mrl->getConToID(), pDest);
                if (target->isInTarget(pDest)) {
                    vector<pair<Cell3DPosition,Vector3D>> blocking = mrl->getBlockingCellsList(pivot);
                    for (pair<Cell3DPosition,Vector3D> p:blocking) {
                        setDistance(p.first,p.second[3]==1?0:1);
                    }
                    vector<pair<DatomsBlock*,PistonId>> blockingDatoms = mrl->getBlockingDatoms(pivot);

                    Deformation d = mrl->getDeformations(module,pivot,blockingDatoms);
                    getScheduler()->schedule(new DeformationStartEvent(scheduler->now()+2000,module,d));
                }
            }
        }
    }
}

void DatomsTestbedCode::onMotionEnd() {
    OUTPUT << "onMotionEnd" << endl;
}
