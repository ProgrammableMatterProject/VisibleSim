#include <queue>
#include <climits>
#include "datomsRotateCode.h"
#include "datomsMotionEngine.h"
bool first=true;

void DatomsRotateCode::startup() {
	lattice = (SkewFCCLattice*)(Datoms::getWorld()->lattice);
	
	if (first) {
		lattice->initTabDistances();
		first=false;
		initDistances();
	}
	
	if (module->blockId==1) {
		tryToMove();
	}
}

void DatomsRotateCode::initDistances() {
	// goal cell =0
	
	assert(target!=NULL);
	short ix,iy,iz;
	Cell3DPosition pos;
	queue<DatomsBlock*> lstModules;
	unsigned int ngoal=0,nwp=0;
    DatomsBlock *datom;
	for (iz=0; iz<lattice->gridSize.pt[2]; iz++) {
		for (iy=0; iy<lattice->gridSize.pt[1]; iy++) {
			for (ix=0; ix<lattice->gridSize.pt[0]; ix++) {
				pos.set(ix,iy,iz);
				// free cell that must be filled
				if (target->isInTarget(pos) && !lattice->cellHasBlock(pos)) {
					lattice->setDistance(pos,0);
					cout << "dist0: " << pos << endl;
					ngoal++;
					
					// goal pivot d=0
					vector<Cell3DPosition> neighbor = lattice->getNeighborhood(pos);
					for (Cell3DPosition p:neighbor) {
						if (lattice->cellHasBlock(p)) {
						    datom = (DatomsBlock*)lattice->getBlock(p);
							lstModules.push(datom);
							lattice->setDistance(p,1);
							datom->setColor(1);
							nwp++;
						}
					}
				}
			}
		}
	}
    DatomsMotionRules *motionRules = DatomsWorld::getWorld()->getMotionRules();
    vector<Cell3DPosition> tabNextCells;
    vector<Cell3DPosition> neighborhood;
    vector<DatomsMotionRulesLink*>tabMRL;
    while (!lstModules.empty()) {
	    datom = lstModules.front(); // pivot
	    lstModules.pop();
        tabNextCells.clear();
        neighborhood = lattice->getNeighborhood(datom->position);
        unsigned short distance = lattice->getDistance(datom->position);
        //OUTPUT << "Selected:" << datom->blockId << ", " << datom->position << ", " << distance << endl;
        for (Cell3DPosition p:neighborhood) {
            if (!lattice->cellHasBlock(p) && lattice->getDistance(p)<distance) {
                short j = datom->getConnectorId(p);
                //OUTPUT << "Neighbor " << p << "#" << j << ": " << lattice->getDistance(p) << endl;
                if (motionRules->getValidMotionList(datom, j, tabMRL)) { // list of theoretical motion
                    for (const DatomsMotionRulesLink *validRule : tabMRL) {
                        Cell3DPosition pDest;
                        datom->getNeighborPos(validRule->getConToID(), pDest);
                        //OUTPUT << "ConTo" << validRule->getConToID() << ":" << pDest << endl;
                        if (!lattice->cellHasBlock(pDest) && lattice->getDistance(pDest) > distance) {
                            tabNextCells.push_back(pDest);
                            lattice->setDistance(pDest, distance);
                        }
                    }
                }
            }
        }


        for (Cell3DPosition cell:tabNextCells) {
            vector<Cell3DPosition> neighbor = lattice->getNeighborhood(cell);
            distance = lattice->getDistance(cell);
            for (Cell3DPosition p:neighbor) {
                if (lattice->cellHasBlock(p)) {
                    datom = (DatomsBlock*)lattice->getBlock(p);
                    lstModules.push(datom);
                    if (lattice->getDistance(p)>distance+1) {
                        lattice->setDistance(p,distance+1);
                        datom->setColor(distance+1);
                        nwp++;
                    }
                }
            }
        }
	}
    cout << "Target: " << nwp << "/" << ngoal << " cells" << endl;
}

bool DatomsRotateCode::tryToMove() {
	vector<std::pair<const DatomsMotionRulesLink*, Deformation>> tab = DatomsMotionEngine::getAllDeformationsForModule(module);
	Cell3DPosition pos;
	unsigned short d,dmin=USHRT_MAX;
	int i=0,imin=-1;
	short n;
	OUTPUT << "Search dest:" << endl;
	for (std::pair<const DatomsMotionRulesLink*, Deformation> v:tab) {
		v.second.getFinalPositionAndOrientation(pos,n);
		OUTPUT << v.second.ptrPivot->blockId << ":" << v.first->getConFromID() << "->" << v.first->getConToID() << "  ";
		OUTPUT << pos << ":";
		d = lattice->getDistance(pos);
		OUTPUT << d << " ";
		if (d<dmin) {
			imin=i;
			dmin=d;
			OUTPUT << "*";
		}
		OUTPUT << endl;
		i++;
	}
	if (imin!=-1) {
		getScheduler()->schedule(new DeformationStartEvent(scheduler->now()+2000,module,tab[imin].second));
	}
	return false;
}

void DatomsRotateCode::onMotionEnd() {
	OUTPUT << "onMotionEnd" << endl;
	if (lattice->getDistance(module->position)!=0) {
		tryToMove();
	}
}

