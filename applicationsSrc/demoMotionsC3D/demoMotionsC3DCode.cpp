#include <climits>
#include "demoMotionsC3DCode.h"


bool distanceCalculated=false;
vector<unsigned int> tabCellByDistance;

// A*
//vector<Cell3DPosition> tabPos = {{11,7,4},{11,7,5},{12,7,5},{13,7,5},{14,6,5},{15,6,5},{16,5,5},{17,5,6}};
// RL no fault
// vector<Cell3DPosition> tabPos = {{11,7,4},{11,7,5},{12,7,5},{13,7,5},{13,7,7},{14,6,7},{15,6,7},{16,6,6},{17,5,6}};
// RL faulty cell at (13,7,7)
// vector<Cell3DPosition> tabPos = {{11,7,4},{11,7,5},{12,7,5},{13,7,5},{14,7,6},{14,6,7},{15,7,8},{16,6,8},{15,6,9},{15,6,7},{16,6,6},{17,5,6}};
// RL faulty cell at (16,6,6)
//vector<Cell3DPosition> tabPos = {{11,7,4},{11,7,5},{12,7,5},{13,7,5},{13,7,7},{14,6,7},{15,6,7},{16,5,7},{17,5,6}};
// RL faulty cell at (12,7,5)
vector<Cell3DPosition> tabPos = {{11,7,4},{11,7,5},{12,8,6},{12,7,7},{13,7,7},{14,6,7},{15,6,7},{16,6,6},{17,5,6}};
size_t currentPosNum=0;

void DemoMotionsCode::startup() {
    lattice = (FCCLattice*)(Catoms3D::getWorld()->lattice);

    if (module->blockId==1) {
        //cout << "position:" << module->position << endl;
        module->setColor(RED);
        tryToMove();
    }
}

bool DemoMotionsCode::tryToMove() {
    vector<std::pair<const Catoms3DMotionRulesLink*, Catoms3DRotation>> tab = Catoms3DMotionEngine::getAllRotationsForModule(module);
    Cell3DPosition pos;
    short n;
    //cout << "Search dest (" << tab.size() << "):" << tabPos[currentPosNum] << endl;
    bool found=false;
    auto it = tab.begin();
    while (it!=tab.end() && !found) {
        it->second.init(((Catoms3DGlBlock*)module->ptrGlBlock)->mat);
        it->second.getFinalPositionAndOrientation(pos,n);
        found = (pos==tabPos[currentPosNum]);
        //cout << "found: " << pos << endl;
        if (found) {
            scheduler->schedule(new Catoms3DRotationStartEvent(getScheduler()->now(), module, it->second.pivot,pos));
        }
        it++;
    }
    return found;
}

void DemoMotionsCode::onMotionEnd() {
    currentPosNum++;
    if (currentPosNum<tabPos.size()) {
        //cout << "position:" << module->position << endl;
        tryToMove();
    }
}
