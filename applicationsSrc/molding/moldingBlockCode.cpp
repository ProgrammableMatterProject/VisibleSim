#include <iostream>
#include "robots/catoms3D/catoms3DWorld.h"
#include "moldingBlockCode.h"

using namespace std;
using namespace Catoms3D;

moldingBlockCode::moldingBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
    // cout << "moldingBlockCode constructor" << endl;
    catom = (Catoms3DBlock*)hostBlock;
}

moldingBlockCode::~moldingBlockCode() {
    //cout << "moldingBlockCode destructor" << endl;
}

void moldingBlockCode::simulatePolymer() {

    if (polymer == NULL) {
        polymer = new Polymer(lattice->gridSize[0],lattice->gridSize[1],4,lattice->gridSize[2]*lattice->gridScale[2],lattice->gridScale[0],lattice->gridScale[0],lattice->gridScale[1]);

        cout << "---------------------------SIMULATION OF THE POLYMER SURFACE-------------------------------" << endl;
        Vector3D pt;
        Catoms3DWorld *world = Catoms3DWorld::getWorld();
        // calculer un table de Zmax
        world->lock();
        for (const auto& pair : world->getMapGl()) {
             Catoms3DGlBlock* c3dGlBlock = (Catoms3DGlBlock*)pair.second;
            if (c3dGlBlock->color[3]!=0) {
                //pt.set((*ic)->position,3);
                pt.pt[0] = c3dGlBlock->position[0];
                pt.pt[1] = c3dGlBlock->position[1];
                pt.pt[2] = c3dGlBlock->position[2];
                polymer->tabPt.push_back(pt);
            }
        }
        world->unlock();
    }

    double v = 0.0; (void) v;
    int i=100;
    do {
        v = polymer->positionInstant(0.01);
        cout << "*";
    } while (i--); //fabs(v)>2.0);
    cout << endl;

    polymer->calculerPolymer();
    cout << "--------------------END OF SIMULATION OF THE POLYMER SURFACE-------------------------------" << endl;
    ofstream file ("polypnts.m");
        file << "polypnts = [" << endl;
        for (int x=0; x <= polymer->_nx ; x++){
            for (int y=0; y <= polymer->_ny ; y++){
                file << polymer->_tabGeom[((y*((polymer->_nx)+1)+x)*6)+3] << " " << polymer->_tabGeom[((y*((polymer->_nx)+1)+x)*6)+4] << " " << polymer->_tabGeom[((y*((polymer->_nx)+1)+x)*6)+5] << ";" << endl;
            }
        }
        file << "]" << endl;
        file.close();
        ofstream file2 ("polygrid.m");
        file2 << "polyx = [" << endl;
        for (int x=0; x <= polymer->_nx ; x++){
            file2 << polymer->_tabGeom[(x*6)+3] << ";" << endl;
        }
        file2 << "];" << endl;
        file2 << "polyy = [" << endl;
    for (int y=0; y <= polymer->_ny ; y++){
            file2 << polymer->_tabGeom[(y*((polymer->_nx)+1)*6)+4] << ";" << endl;
        }
        file2 << "];" << endl;
        file2.close();
        cout << "file written" << endl;
}


void moldingBlockCode::onGlDraw() {
    if (polymer) {
        polymer->glDraw();
    }
}

void moldingBlockCode::onUserKeyPressed(unsigned char c, int x, int y) {
    switch (c) {
        case 'p' :
            // FIXME: Fix this, it should not be part of the simulatorCore
            simulatePolymer();
            break;
    }
}

void moldingBlockCode::startup() {
    // Change Color
    if (target->isInTarget(catom->position)){
        catom->setColor(GREEN);
    }
    else{
        catom->setColor(RED);
        catom->setVisible(false);
    }
}
