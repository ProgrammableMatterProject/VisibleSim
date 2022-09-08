#include "myMotionTestCode.hpp"
#include "robots/catoms3D/catoms3DMotionEngine.h"

MyMotionTestCode::MyMotionTestCode(Catoms3DBlock *host) : Catoms3DBlockCode(host), module(host)
{
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host)
        return;
}

void MyMotionTestCode::startup()
{
    console << "start " << module->blockId << "\n";
    if (module->blockId == 11)
    { // Master id is 1
        module->setColor(RED);
        Cell3DPosition target_pos = module->position + Cell3DPosition(-1, -1, -1);
        module->moveTo(target_pos);

        vector<std::pair<const Catoms3DMotionRulesLink*, Catoms3DRotation>> tab = Catoms3DMotionEngine::getAllRotationsForModule(module);
        Cell3DPosition finalPos;
        short finalOrient;
        cout << "List:" << endl;
        for(auto &elem:tab) {
            elem.second.init(((Catoms3DGlBlock *) module->ptrGlBlock)->mat);
            elem.second.getFinalPositionAndOrientation(finalPos, finalOrient);
            if (lattice->isFree(finalPos)) {
                cout << finalPos << endl;
            }
        }
        cout << "Endoflist" << endl;
    }
}

void MyMotionTestCode::parseUserBlockElements(TiXmlElement *config)
{
    const char *attr = config->Attribute("leader");
    if (attr != nullptr)
    {
        std::cout << module->blockId << " is leader!" << std::endl; // complete with your code
    }
}

void MyMotionTestCode::onMotionEnd()
{
    // complete with your code
    console << " End of motion to " << module->position << "\n";
}