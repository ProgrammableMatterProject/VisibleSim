#include <climits>
#include "robots/catoms3D/catoms3DRotateCode.h"


bool distanceCalculated=false;
vector<unsigned int> tabCellByDistance;

void Catoms3DRotateCode::startup() {
    lattice = (FCCLattice*)(Catoms3D::getWorld()->lattice);

    if (module->blockId==1) {
        lattice->initTabDistances();
        lattice->setDistance(module->position,0);
        tabCellByDistance.push_back(1);
        initDistances();
    }

}

void Catoms3DRotateCode::initDistances() {
    short distance=lattice->getDistance(module->position)+1;
    vector<Cell3DPosition> neighbor = lattice->getNeighborhood(module->position); //
    Cell3DPosition pDest;
    short r;
    vector<std::pair<const Catoms3DMotionRulesLink*, Rotations3D>> tab = Catoms3DMotionEngine::getAllRotationsForModule(module);

    for (auto p:tab) {
        p.second.init(((Catoms3DGlBlock*)module->ptrGlBlock)->mat);
        p.second.getFinalPositionAndOrientation(pDest,r);
        unsigned short d = lattice->getDistance(pDest);
        if (d>distance) {
            if (d!=USHRT_MAX) {
                tabCellByDistance[d]--;
            }
            if (tabCellByDistance.size()<=distance) {
                tabCellByDistance.push_back(0);
            }
            tabCellByDistance[distance]++;
            lattice->setDistance(pDest,distance);
            cellsList.push(pDest);
        }
    }

    if (!cellsList.empty()) {
        pDest = cellsList.front();
        cellsList.pop();
        getScheduler()->schedule(new TeleportationStartEvent(scheduler->now()+2000,module,pDest));
    } else {
        distanceCalculated=true;
        pDest.set(0,0,2);
        lattice->pushPtLine(pDest);
        getScheduler()->schedule(new TeleportationStartEvent(scheduler->now()+2000000,module,pDest));
    }
}

bool Catoms3DRotateCode::tryToMove() {
    vector<std::pair<const Catoms3DMotionRulesLink*, Rotations3D>> tab = Catoms3DMotionEngine::getAllRotationsForModule(module);
    Cell3DPosition pos;
    unsigned short d,dmin=USHRT_MAX;
    int i=0,imin=-1;
    short n;
    OUTPUT << "Search dest:" << endl;
    for (auto v:tab) {
        v.second.getFinalPositionAndOrientation(pos,n);
        //OUTPUT << v.second.pivot->blockId << ":" << v.first->getConFromID() << "->" << v.first->getConToID() << "  ";
        //OUTPUT << pos << ":";
        d = lattice->getDistance(pos);
        //OUTPUT << d << " ";
        if (d<dmin) {
            imin=i;
            dmin=d;
            //OUTPUT << "*";
        }
        //OUTPUT << endl;
        i++;
    }
    if (imin!=-1) {
        /*currentMotion->MRlist->sendRotationEvent(module,currentMotion->fixed,scheduler->now()+2000);
        getScheduler()->schedule(new DeformationStartEvent(scheduler->now()+2000,module,tab[imin].second));*/
    }
    return false;
}

void Catoms3DRotateCode::onMotionEnd() {
    //OUTPUT << "onMotionEnd" << endl;
    if (distanceCalculated) {
        int i=0;
        for (unsigned int t:tabCellByDistance) {
            cout << i << ": " << t << endl;
            i++;
        }

        /*lattice->showTabDistances(false);
         *		if (lattice->getDistance(module->position)>0) {
         *			tryToMove();
    }
    lattice->ptsLine.push_back(module->position);*/
    } else {
        initDistances();
    }
}
