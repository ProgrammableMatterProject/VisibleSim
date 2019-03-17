#include <queue>
#include <climits>
#include "datomsTestbedCode.h"
#include "datomsMotionEngine.h"

void DatomsTestbedCode::startup() {
	lattice = (SkewFCCLattice*)(Datoms::getWorld()->lattice);

	if (module->blockId==1) { // mobile
	    lattice->initTabDistances();

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
                        lattice->setDistance(p.first,p.second[3]==1?0:1);
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

