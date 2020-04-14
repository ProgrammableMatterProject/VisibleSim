#include <climits>
#include "demoMotionsC3DCode.h"


bool distanceCalculated=false;
vector<unsigned int> tabCellByDistance;

vector<Cell3DPosition> tabPos = {{6,2,3},{5,2,3},{5,3,3},{4,3,3},{3,3,3},{3,2,3},{2,2,3},{2,1,3},{2,1,2},{2,1,1}};
size_t currentPosNum=0;

void DemoMotionsCode::startup() {
    lattice = (FCCLattice*)(Catoms3D::getWorld()->lattice);

    if (module->blockId==54) {
        cout << "position:" << module->position << endl;
        module->setColor(RED);
        tryToMove();
    }
}

bool DemoMotionsCode::tryToMove() {
    vector<std::pair<const Catoms3DMotionRulesLink*, Catoms3DRotation>> tab = Catoms3DMotionEngine::getAllRotationsForModule(module);
    Cell3DPosition pos;
    short n;
    cout << "Search dest (" << tab.size() << "):" << tabPos[currentPosNum] << endl;
    bool found=false;
    auto it = tab.begin();
    while (it!=tab.end() && !found) {
        it->second.init(((Catoms3DGlBlock*)module->ptrGlBlock)->mat);
        it->second.getFinalPositionAndOrientation(pos,n);
        found = (pos==tabPos[currentPosNum]);
        cout << "found: " << pos << endl;
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
        cout << "position:" << module->position << endl;
        tryToMove();
    }
}
