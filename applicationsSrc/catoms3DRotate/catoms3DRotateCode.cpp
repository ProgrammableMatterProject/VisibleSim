#include <climits>
#include "catoms3DRotateCode.h"

bool distanceCalculated=false;
vector<unsigned int> tabCellByDistance;

void Catoms3DRotateCode::startup() {
    lattice = (FCCLattice*)(Catoms3D::getWorld()->lattice);
    if (isMobile) {
        previousCellsList.push_front(module->position);
        tryToMove();
    }
}

bool Catoms3DRotateCode::tryToMove() {
    vector<std::pair<const Catoms3DMotionRulesLink*, Catoms3DRotation>> tab = Catoms3DMotionEngine::getAllRotationsForModule(module);

    Cell3DPosition finalPos;
    short finalOrient;
    Catoms3DRotation bestRotation;
    short bestDistance=-1;
    cout << "possible motions :" << tab.size() << endl;
    for(auto &elem:tab) {
        elem.second.init(((Catoms3DGlBlock*)module->ptrGlBlock)->mat);
        elem.second.getFinalPositionAndOrientation(finalPos,finalOrient);
        if (lattice->isInGrid(finalPos) && lattice->isFree(finalPos)) {
            /*rotateBlockSubMenu->addButton(new GlutRotationButton(NULL,i++,0,0,0,0,menuDir+"menu_link.tga",
                                                                 elem.first->isOctaFace(),elem.first->getConFromID(),elem.first->getConToID(),finalPos,finalOrient));*/
            cout << "possible motions:" << finalPos << endl;
            if (target->isInTarget(finalPos)) {
                cout << "is in target:" << finalPos << endl;
                auto it = std::find(previousCellsList.begin(), previousCellsList.end(), finalPos);
                if (it==previousCellsList.end())  { // pas dans la liste
                    previousCellsList.push_front(finalPos);
                    cout << "motion to ..." << finalPos << endl;
                    scheduler->schedule(new Catoms3DRotationStartEvent(getScheduler()->now(), module, elem.second.pivot,finalPos));
                    return true;
                } else {
                    int distance = it - previousCellsList.begin();
                    cout << "is in position:" << distance << endl;
                    if (distance>bestDistance) {
                        bestDistance=distance;
                        bestRotation=elem.second;
                    }
                }
            }
        }
    }
    if (bestDistance!=-1) {
        //bestRotation.init(((Catoms3DGlBlock*)module->ptrGlBlock)->mat);
        bestRotation.getFinalPositionAndOrientation(finalPos,finalOrient);
        cout << "Best distance=" << bestDistance << " -> " << finalPos << endl;
        auto it = std::find(previousCellsList.begin(), previousCellsList.end(), finalPos);
        previousCellsList.erase(it);
        scheduler->schedule(new Catoms3DRotationStartEvent(getScheduler()->now(), module, bestRotation.pivot,finalPos));
        cout << "motion to ..." << finalPos << endl;
        previousCellsList.push_front(finalPos);
        return true;
    }
    return false;
}

void Catoms3DRotateCode::onMotionEnd() {
    cout << "onMotionEnd" << endl;
    tryToMove();
}

void Catoms3DRotateCode::parseUserBlockElements(TiXmlElement *config) {
    const char *attr = config->Attribute("mobile");
    if (attr!=nullptr) {
        string str(attr);
        if (str=="true" || str=="1" || str=="yes") {
            isMobile=true;
            std::cout << module->blockId << " is mobile!" << std::endl; // complete with your code
        }
    }
}