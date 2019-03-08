#include <iostream>
#include "catoms3DWorld.h"
#include "targetColorationBlockCode.h"

using namespace std;
using namespace Catoms3D;

targetColorationBlockCode::targetColorationBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
    cout << "targetColorationBlockCode constructor" << endl;
    catom = (Catoms3DBlock*)hostBlock;
}

targetColorationBlockCode::~targetColorationBlockCode() {
    //cout << "targetColorationBlockCode destructor" << endl;
}

void targetColorationBlockCode::onUserKeyPressed(unsigned char c, int x, int y) {
    switch (c) {
        case 'p' :
            // FIXME: Fix this, it should not be part of the simulatorCore
            BaseSimulator::getWorld()->simulatePolymer();
            break;
    }        
}

void targetColorationBlockCode::startup() {
    // Change Color
    if (target->isInTarget(catom->position)){
        catom->setColor(GREEN);
    }
    else{
        catom->setColor(RED);
        catom->setVisible(false);
    }
}
