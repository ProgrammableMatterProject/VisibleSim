#include "robots/nodes2D/nodes2DMotionEngine.h"

namespace Nodes2D {

Nodes2DMotionEngine::Nodes2DMotionEngine() {
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::South,Cell3DPosition(0,-1,0),CCW,Cell3DPosition(-1,-1,0),true,Cell3DPosition(-1,0,0))); // South, translation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::South,Cell3DPosition(0,-1,0),CCW,Cell3DPosition(-1,-1,0),false,Cell3DPosition(-1,0,0))); // South, rotation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::East,Cell3DPosition(+1,0,0),CCW,Cell3DPosition(+1,-1,0),true,Cell3DPosition(0,-1,0))); // East, translation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::East,Cell3DPosition(+1,0,0),CCW,Cell3DPosition(+1,-1,0),false,Cell3DPosition(0,-1,0))); // East, rotation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::North,Cell3DPosition(0,+1,0),CCW,Cell3DPosition(+1,+1,0),true,Cell3DPosition(+1,0,0))); // North, translation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::North,Cell3DPosition(0,+1,0),CCW,Cell3DPosition(+1,+1,0),false,Cell3DPosition(+1,0,0))); // North, rotation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::West,Cell3DPosition(-1,0,0),CCW,Cell3DPosition(-1,+1,0),true,Cell3DPosition(0,+1,0))); // West, translation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::West,Cell3DPosition(-1,0,0),CCW,Cell3DPosition(-1,+1,0),false,Cell3DPosition(0,+1,0))); // West, rotation

        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::South,Cell3DPosition(0,-1,0),CW,Cell3DPosition(+1,-1,0),true,Cell3DPosition(1,0,0))); // South, translation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::South,Cell3DPosition(0,-1,0),CW,Cell3DPosition(+1,-1,0),false,Cell3DPosition(1,0,0))); // South, rotation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::East,Cell3DPosition(+1,0,0),CW,Cell3DPosition(+1,+1,0),true,Cell3DPosition(0,1,0))); // East, translation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::East,Cell3DPosition(+1,0,0),CW,Cell3DPosition(+1,+1,0),false,Cell3DPosition(0,1,0))); // East, rotation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::North,Cell3DPosition(0,+1,0),CW,Cell3DPosition(-1,+1,0),true,Cell3DPosition(-1,0,0))); // North, translation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::North,Cell3DPosition(0,+1,0),CW,Cell3DPosition(-1,+1,0),false,Cell3DPosition(-1,0,0))); // North, rotation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::West,Cell3DPosition(-1,0,0),CW,Cell3DPosition(-1,-1,0),true,Cell3DPosition(0,-1,0))); // West, translation
        tabNodes2DMotions.push_back(new Nodes2DMotion(SLattice::West,Cell3DPosition(-1,0,0),CW,Cell3DPosition(-1,-1,0),false,Cell3DPosition(0,-1,0))); // West, rotation
    }

Nodes2DMotionEngine::~Nodes2DMotionEngine() {
    for (Nodes2DMotion*nm:tabNodes2DMotions) {
        delete nm;
    }
    tabNodes2DMotions.clear();
}

vector<Nodes2DMotion*> Nodes2DMotionEngine::getAllMotionsForModule(BuildingBlock *nb,const SLattice*sl) {
    Cell3DPosition pos;
    bool hasObs,hasPivot,hasFreeDest;
    vector<Nodes2DMotion*>res;

    for (Nodes2DMotion*nm:tabNodes2DMotions) {
        hasObs=!sl->isFree(nm->obstaclePos+nb->position);
        pos = nm->pivotPos+nb->position;
        hasPivot=!sl->isFree(pos) && sl->isInGrid(pos);
        pos = nm->finalPos+nb->position;
        hasFreeDest=sl->isFree(pos) && sl->isInGrid(pos) && (sl->isFree(nm->pathPos+nb->position) || !nm->isRotation);
        //cout << "motion:" << nm->fromConId << "->" << nm->toConId << " hasPivot=" << hasPivot << " hasFreeDest=" << hasFreeDest << " hasObs=" << hasObs << "/" << nm->hasObstacle <<endl;
        if (hasPivot && hasFreeDest && hasObs==nm->hasObstacle) {
            res.push_back(nm);
        }
    }
    return res;
}

}
