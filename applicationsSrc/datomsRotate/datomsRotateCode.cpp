#include <climits>
#include "datomsRotateCode.h"
#include "robots/datoms/datomsMotionEngine.h"

bool first=true;

void DatomsRotateCode::initTabLockedCells() {
    if (tabLockedCells == nullptr) {
        const Cell3DPosition& gs = lattice->gridSize;
        int n = gs.pt[0]*gs.pt[1]*gs.pt[2];
        tabLockedCells = new bool[n];
        // initialisation of tabLockedCells with value false
        bool *ptr = tabLockedCells;
        while (n--) {
            *ptr++ = false;
        }
    }
}

void DatomsRotateCode::initTabDistances() {
    if (tabDistances == NULL) {
        const Cell3DPosition& gs = lattice->gridSize;
        int n = gs.pt[0]*gs.pt[1]*gs.pt[2];
        tabDistances = new unsigned short[n];
        // initialisation of tabDistances with value 'd'
        unsigned short *ptr = tabDistances;
        while (n--) {
            *ptr++=USHRT_MAX;
        }
    }
}

unsigned short DatomsRotateCode::getDistance(const Cell3DPosition &pos) {
    if (!lattice->isInGrid(pos)) return USHRT_MAX;
    return tabDistances[lattice->getIndex(pos)];
}

void DatomsRotateCode::setDistance(const Cell3DPosition &pos,unsigned short d) {
    if (lattice->isInGrid(pos)) tabDistances[lattice->getIndex(pos)]=d;
}

void DatomsRotateCode::startup() {
    lattice = (SkewFCCLattice*)(Datoms::getWorld()->lattice);

    if (module->blockId==1) {
        initTabLockedCells();
        initTabDistances();
        setDistance(module->position,0);
        initDistances();
    }

}

void DatomsRotateCode::initDistances() {
    short distance=getDistance(module->position)+1;
    vector<Cell3DPosition> neighbor = lattice->getNeighborhood(module->position); //
    vector<DatomsMotionRulesLink*>tabMRL;
    Cell3DPosition pDest;
    short r;
    vector<std::pair<const DatomsMotionRulesLink*, Deformation>> tab = DatomsMotionEngine::getAllDeformationsForModule(module);
    for (auto p:tab) {
        p.second.getFinalPositionAndOrientation(pDest,r);
        if (getDistance(pDest)>distance) {
            OUTPUT << "--> new Distance" << pDest << "," << distance << endl;
            setDistance(pDest,distance);
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
        ptsLine.push_back(pDest);
        getScheduler()->schedule(new TeleportationStartEvent(scheduler->now()+2000000,module,pDest));
    }

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
        d = getDistance(pos);
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
    if (distanceCalculated) {
        // showTabDistances(false);
        if (getDistance(module->position)>0) tryToMove();
        ptsLine.push_back(module->position);
    } else {
        initDistances();
    }
}

bool DatomsRotateCode::lockCell(const Cell3DPosition &pos) {
    if (!lattice->isInGrid(pos)) return true;

    int ind = lattice->getIndex(pos);
    OUTPUT << "ind=" << ind << " / pos=" << pos << " / state=" << tabLockedCells[ind] << " / bb=" << (lattice->grid[ind]==NULL?0:1) << endl;
    if (tabLockedCells[ind] || lattice->grid[ind]!=NULL) {
        return false;
    }
    tabLockedCells[ind] = true;
    return true;
}

bool DatomsRotateCode::unlockCell(const Cell3DPosition &pos) {
    if (!lattice->isInGrid(pos)) return true;

    int ind = lattice->getIndex(pos);
    bool prev = tabLockedCells[ind];
    tabLockedCells[ind] = false;
    return prev;
}

void DatomsRotateCode::onGlDraw() {
    static const GLfloat white[]={0.8f,0.8f,0.8f,1.0f}, gray[]={0.4f,0.4f,0.4f,1.0f}, red[]={1.0f,0.2f,0.2f,1.0f};

    if (tabDistances && showDistance) {
        short ix,iy,iz,decz;
        Cell3DPosition gp;
        Vector3D v;
        unsigned short *ptrDistance = tabDistances;
        bool *ptr = tabLockedCells;
        const Cell3DPosition& gridSize = lattice->gridSize;
        const Cell3DPosition& gridScale = lattice->gridScale;

        glMaterialfv(GL_FRONT,GL_AMBIENT,gray);
        glMaterialfv(GL_FRONT,GL_DIFFUSE,white);
        glMaterialfv(GL_FRONT,GL_SPECULAR,white);
        glMaterialf(GL_FRONT,GL_SHININESS,40.0);
        for (iz=0; iz<gridSize[2]; iz++) {
            decz=iz/2;
            for (iy=-decz; iy<gridSize[1]-decz; iy++) {
                for (ix=-decz; ix<gridSize[0]-decz; ix++) {
                    gp.set(ix,iy,iz);
                    // index = getIndex(gp);
                    if (*ptr) {
                        glPushMatrix();
                        v = lattice->gridToWorldPosition(gp);
                        glTranslatef(v[0],v[1],v[2]);
                        glutSolidSphere(0.065*gridScale[0],6,6);
                        glPopMatrix();
                    }
                    // if (*ptrDistance!=USHRT_MAX && grid[index]==NULL) {
                    if (*ptrDistance!=USHRT_MAX) {
                        glPushMatrix();
                        v = lattice->gridToWorldPosition(gp);
                        glTranslatef(v[0],v[1],v[2]);

                        glMaterialfv(GL_FRONT,GL_DIFFUSE,tabColors[*ptrDistance%12]);
                        glScalef(1.0f,1.0f,0.1f);
                        glRotatef(90.0f,0,0,1.0f);
                        glutSolidCube(0.5*gridScale[0]);
                        //glutSolidSphere(0.25*gridScale[0],12,12);
                        glPopMatrix();
                    }
                    ptr++;
                    ptrDistance++;
                }
            }
        }
    }
    if (ptsLine.size()>1) {
        glMaterialfv(GL_FRONT,GL_AMBIENT,red);
        glMaterialfv(GL_FRONT,GL_DIFFUSE,red);
        glMaterialfv(GL_FRONT,GL_SPECULAR,white);
        glMaterialf(GL_FRONT,GL_SHININESS,40.0);
        glColor3f(1.0f,0,0);
        Vector3D p,q,v;
        double d,phi,theta;
        vector<Cell3DPosition>::const_iterator ci=ptsLine.begin();
        while ((ci+1)!=ptsLine.end()) {
            p = lattice->gridToWorldPosition(*ci);
            q = lattice->gridToWorldPosition(*(ci+1));
            glPushMatrix();
            glTranslatef(p[0],p[1],p[2]);
            glutSolidSphere(1.0,6,6);
            v = q-p;
            d = v.norme();
            v.normer_interne();
            phi = asin(v[2]);
            theta = (v[0]>0)?asin(v[1]/cos(phi)):M_PI-asin(v[1]/cos(phi));
            glRotatef(theta*180.0/M_PI,0,0,1.0);
            glRotatef(90.0-phi*180.0/M_PI,0,1.0,0);
            glutSolidCylinder(0.75,d,12,1);
            glPopMatrix();
            ci++;
        }
    }
}
