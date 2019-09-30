#include "datomsMotionRules.h"
#include "deformationEvents.h"
#include "datomsWorld.h"
#include "datomsMotionEngine.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace Datoms
namespace Datoms {

const int tabConnectors4[6][4] = { {0,1,2,10},{1,3,6,11},{4,6,7,8},{0,5,7,9},{8,9,10,11},{2,3,4,5}};

const short neighborConnector[12][6] = {
    { 5, 2, 1, 10, 9, 7 }, // Con0 UP
    { 11, 10, 0, 2, 3, 6 }, // Con1 DOWN
    { 0, 5, 4, 3, 1, 10 }, // Con2 LEFT
    { 4, 6, 11, 1, 2, 5 }, // Con3 RIGHT
    { 6, 3, 2, 5, 7, 8 }, // Con4 LEFT
    { 2, 0, 9, 7, 4, 3  }, // Con5 RIGHT
    { 3, 4, 7, 8, 11, 1  }, // Con6 UP
    { 9, 8, 6, 4, 5, 0 }, // Con7 DOWN
    { 7, 9, 10, 11, 6, 4 }, // Con8 RIGHT
    { 8, 7, 5, 0, 10, 11 }, // Con9 LEFT
    { 1, 11, 8, 9, 0, 2 }, // Con10 RIGHT
    { 10, 1, 3, 6, 8, 9 }, // Con11 LEFT
};

const ConnectorOrientation defaultOrientation[12] = {
    UP, DOWN, LEFT, RIGHT, LEFT, RIGHT, UP, DOWN, RIGHT, LEFT, RIGHT, LEFT
};

ConnectorDirection
DatomsMotionRules::getMirrorConnectorDirection(ConnectorDirection d,
                                                 bool inverted) {
    switch (d) {
        case NORTH_WEST: return inverted ? SOUTH_WEST : NORTH_EAST;
        case NORTH_EAST: return inverted ? SOUTH_EAST : NORTH_WEST;
        case SOUTH_WEST: return inverted ? NORTH_WEST : SOUTH_EAST;
        case SOUTH_EAST: return inverted ? NORTH_EAST : SOUTH_WEST;
        case EAST: return inverted ? EAST : WEST;
        case WEST: return inverted ? WEST : EAST;
        default:
            throw InvalidArgumentException(__PRETTY_FUNCTION__,
                                           std::string("d = ") + to_string(d));
    }
}

short DatomsMotionRules::getConnectorDirection(short anchorCon, short conTo) {
    if (anchorCon < 12 and anchorCon >= 0 and conTo < 12 and conTo >= 0) {
        for (short conDir = 0; conDir < NUM_CONDIRS; conDir++)
            if (neighborConnector[anchorCon][conDir] == conTo) return conDir;
    }

    return -1;
}

short DatomsMotionRules::getMirrorNeighborConnector(short conFrom, ConnectorDirection d,
                                                      bool inverted) {
    return conFrom >= 12 ?
        -1 : neighborConnector[conFrom][getMirrorConnectorDirection(d, inverted)];
}

short DatomsMotionRules::getNeighborConnector(short conFrom, ConnectorDirection d) {
    return conFrom >= 12 ? -1 : neighborConnector[conFrom][d];
}

const short *DatomsMotionRules::getNeighborConnectors(short conFrom) {
    return conFrom >= 12 ? NULL : neighborConnector[conFrom];
}

const vector<DatomsMotionRulesLink*>&
DatomsMotionRules::getMotionRulesLinksForConnector(short con) {
    if (con < 0 || con > 11)
        throw InvalidArgumentException(__PRETTY_FUNCTION__,
                                       std::string("con = ") + to_string(con));

    return tabConnectors[con]->tabLinks;
}


DatomsMotionRules::DatomsMotionRules() {

// allocation of connectors
    for (int i=0; i<12; i++) {
        tabConnectors[i] = new DatomsMotionRulesConnector(i);
    }

    Vector3D up(0,+1,0),upleft(+1,+1,+M_SQRT2),upright(-1,+1,-M_SQRT2),
             down(0,-1,0),downright(+1,-1,-M_SQRT2),downleft(-1,-1,+M_SQRT2),left(0,0,1),right(0,0,-1),
             lup(-1,+1,0),rup(+1,+1,0);
    addLinks4(0,2,1,10,left,lup,rup,2);
    addLinks4(6,4,7,8,left,lup,rup,4);
    addLinks4(1,3,6,11,right,-rup,-lup,3);
    addLinks4(7,5,0,9,right,-rup,-lup,5);
    addLinks4(2,5,4,3,left,lup,rup,6);
    addLinks4(8,9,10,11,right,-rup,-lup,7);
}

DatomsMotionRules::~DatomsMotionRules() {
    for (int i=0; i<12; i++) {
        delete tabConnectors[i];
    }
}

bool DatomsMotionRulesLink::concernsConnector(short conId) const {
    return getConFromID() == conId || getConToID() == conId;
}

bool DatomsMotionRulesLink::concernsConnectors(short conId1, short conId2) const {
    return concernsConnector(conId1) && concernsConnector(conId2);
}

string DatomsMotionRulesLink::getID() {
    string c="X->X";
    uint8_t id1 = conFrom->ID;
    uint8_t id2 = conTo->ID;
    char ch = (id1<10?'0'+id1:'A'+id1-10);
    c[0]=ch;
    ch = (id2<10?'0'+id2:'A'+id2-10);
    c[3]=ch;
    return c;
}

const int *findTab4(int id1,int id2) {
    int i=0,j;
    bool id1found=false,id2found=false;
    while (i<6 && !(id1found && id2found)) {
        id1found=false;
        id2found=false;
        for (j=0; j<4; j++) {
            if (id1==tabConnectors4[i][j]) id1found=true;
            if (id2==tabConnectors4[i][j]) id2found=true;
        }
        i++;
    }
    assert(id1found && id2found);
    return tabConnectors4[i-1];
}

void DatomsMotionRules::addLinks4(int id1, int id2, int id3, int id4, const Vector3D &left,const Vector3D &lup,const Vector3D &rup,uint8_t modelId) {
    int tabBC1[4],tabBC2[4],tabBC3[4],tabBC4[4];

    tabBC1[0]=id2;	tabBC1[1]=id3;  tabBC1[2]=id4; //tabBC1[3] = (id1+6)%12;
    tabBC2[0]=id1;	tabBC2[1]=id3;  tabBC2[2]=id4; //tabBC2[3] = (id2+6)%12;
    tabBC3[0]=id2;	tabBC3[1]=id1;  tabBC3[2]=id4; //tabBC3[3] = (id3+6)%12;
    tabBC4[0]=id2;	tabBC4[1]=id3;  tabBC4[2]=id1; //tabBC4[3] = (id4+6)%12;

/* 1->2 & 2->1*/
    addLink(OctaFace,id1,id2,-left,rup,3,tabBC1,modelId);
    addLink(OctaFace,id2,id1,left,lup,3,tabBC2,modelId);
/* 1->3 & 3->1*/
    addLink(OctaFace,id1,id3,-left,-left,3,tabBC1,modelId);
    addLink(OctaFace,id3,id1,-left,-left,3,tabBC3,modelId);
/* 1->4 & 4->1*/
    addLink(OctaFace,id1,id4,-left,-rup,3,tabBC1,modelId);
    addLink(OctaFace,id4,id1,left,-lup,3,tabBC4,modelId);
/* 2->3 & 3->2*/
    addLink(OctaFace,id2,id3,left,-lup,3,tabBC2,modelId);
    addLink(OctaFace,id3,id2,-left,-rup,3,tabBC3,modelId);
/* 2->4 & 4->2*/
    addLink(OctaFace,id2,id4,left,left,3,tabBC2,modelId);
    addLink(OctaFace,id4,id2,left,left,3,tabBC4,modelId);
/* 3->4 & 4->3*/
    addLink(OctaFace,id3,id4,-left,rup,3,tabBC3,modelId);
    addLink(OctaFace,id4,id3,left,lup,3,tabBC4,modelId);
}


void DatomsMotionRules::addLink(DeformationLinkType  mrlt,int id1, int id2,const Vector3D &axis1,const Vector3D &axis2,int n, int *tabBC,uint8_t modelId) {
    Matrix M = DatomsBlock::getMatrixFromPositionAndOrientation(Cell3DPosition(0,0,0),id1);
    Matrix M_1;
    M.inverse(M_1);
    Vector3D ax1 = M_1*axis1;
    Vector3D ax2 = M_1*axis2;
    ax1.normer_interne();
    ax2.normer_interne();

    DatomsMotionRulesLink *lnk = new DatomsMotionRulesLink(mrlt,tabConnectors[id1],tabConnectors[id2],ax1,ax2,modelId);
    tabConnectors[id1]->addLink(lnk);
//    OUTPUT << id1 << " -> " << id2 << endl;
    for (int i=0; i<n; i++) {
        if (tabBC[i]!=id2) {
            lnk->addBlockingConnector(tabBC[i]);
//            OUTPUT << tabBC[i] << " ";
        }
    }
//    OUTPUT << endl;
}

bool DatomsMotionRules::getValidMotionList(const DatomsBlock* c3d,int from,vector<DatomsMotionRulesLink*>&vec) {
    DatomsMotionRulesConnector *conn = tabConnectors[from];
    bool notEmpty=false;

    vector <DatomsMotionRulesLink*>::const_iterator ci=conn->tabLinks.begin();
    while (ci!=conn->tabLinks.end()) {
        OUTPUT << from << " -> " << (*ci)->getConToID() ;
        if ((*ci)->isValid(c3d)) {
            vec.push_back(*ci);
            notEmpty=true;
            OUTPUT << "(valid)" << endl;
        }
        ci++;
    }
    return notEmpty;
}

bool DatomsMotionRules::getValidSurfaceLinksOnDatom(const DatomsBlock* pivot,
                                                      vector<DatomsMotionRulesLink*>& links) {
    Lattice *lattice = DatomsWorld::getWorld()->lattice;

    for (short from = 0; from < 12; from++) {
        DatomsMotionRulesConnector *con = tabConnectors[from];
        Cell3DPosition fromAdjPos;

        // We do not consider the source conector as blocking since their could be a moving module on there, but all the other ones are potentially blocking
        pivot->getNeighborPos(from, fromAdjPos);

        for (DatomsMotionRulesLink *link : con->tabLinks) {
            int conTo = link->getConToID();
            bool isOk = false;
            Cell3DPosition pos;

            // Destination must be in lattice and free
            pivot->getNeighborPos(conTo,pos);
            isOk = lattice->isInGrid(pos) && !lattice->cellHasBlock(pos);

            // list of cells that must be free
            if (link->isOctaFace()) {
                const int *ptr = findTab4(from,conTo);
                int i=0;
                while (isOk && i<4) {
                    if (ptr[i]!=from && ptr[i]!=conTo) {
                        pivot->getNeighborPos(ptr[i],pos);
                        isOk = !lattice->cellHasBlock(pos);
                    }
                    i++;
                }
            } /*else { // isHexaFace
                const int *ptr = findTab3(from,conTo);
                int i=0;
                while (isOk && i<3) {
                    if (ptr[i]!=from && ptr[i]!=conTo) {
                        datom->getNeighborPos(ptr[i],pos);
                        isOk = !lattice->cellHasBlock(pos);
                    }
                    i++;
                }
            }*/

            if (isOk) {
                links.push_back(link);
                // OUTPUT << "ADD " << from << " -> " << conTo << endl;
            }
        }
    } // for

    return !links.empty();
}


bool DatomsMotionRules::getValidMotionListFromPivot(const DatomsBlock* pivot,int from,
                                                    vector<DatomsMotionRulesLink*>&vec,
                                                    const SkewFCCLattice *lattice,
                                                    const Target *target) {
    DatomsMotionRulesConnector *conn = tabConnectors[from];
    bool notEmpty=false;
    bool isOk;
    Cell3DPosition pos,pos2;

    // source must be free or is not in goal
    pivot->getNeighborPos(from,pos);
//OUTPUT << "FROM pos=" << pos << endl;
    if (lattice->cellHasBlock(pos) && target->isInTarget(pos)) return false;
    pos2 = 2*pos + pos - pivot->position;
//OUTPUT << "OPP FROM pos=" << pos2 << endl;
    if (lattice->cellHasBlock(pos2)) return false;

    vector <DatomsMotionRulesLink*>::const_iterator ci=conn->tabLinks.begin();
    while (ci!=conn->tabLinks.end()) {
        int to = (*ci)->getConToID();
        //OUTPUT << from << " -> " << to << ", ";
        // destination must be in lattice and free
        pivot->getNeighborPos(to,pos);
//OUTPUT << "TO pos=" << pos << endl;
        isOk = lattice->isInGrid(pos) && !lattice->cellHasBlock(pos);
        // opposite cell must be free
        pos2 = 2*pos - pivot->position;
//OUTPUT << "OPP TO pos=" << pos2 << endl;
        isOk = isOk && !lattice->cellHasBlock(pos2);

        // list of cells that must be free
        if ((*ci)->isOctaFace()) {
            const int *ptr = findTab4(from,to);
            int i=0;
            while (isOk && i<4) {
                if (ptr[i]!=from && ptr[i]!=to) {
                    pivot->getNeighborPos(ptr[i],pos);
                    isOk = !lattice->cellHasBlock(pos);
                }
                i++;
            }
        }
        if (isOk) {
            vec.push_back(*ci);
            //OUTPUT << "ADD " << from << " -> " << to << endl;
            notEmpty=true;
        }
        ci++;
    }
    return notEmpty;
}

const DatomsMotionRulesLink* DatomsMotionRules::
getMobileModuleLinkMatchingPivotLink(const DatomsMotionRulesLink* pivLink,
                                     const DatomsBlock* m, const DatomsBlock* pivot) {
    if (pivLink and m and pivot) {
        short conFrom = m->getConnectorId(pivot->position);
        short conTo =
            DatomsMotionEngine::getMirrorConnectorOnModule(pivot, m,                                                      pivLink->getConFromID(),conFrom, pivLink->getConToID());

        // cout << "Conconv: #" << pivot->blockId << " l: " << *pivLink <<endl;
        // cout << pivLink->getConFromID() << " === " << conFrom << endl;
        // cout << pivLink->getConToID() << " === " << conTo << endl;

        vector<DatomsMotionRulesLink*> motionRulesLinksFrom;
        getValidMotionList(m, conFrom, motionRulesLinksFrom);

        for (const DatomsMotionRulesLink* link : motionRulesLinksFrom) {
            if (link->getConToID() == conTo
                and link->getMRLT() == pivLink->getMRLT())
                return link;
        }
    }

    return NULL;
}

void DatomsMotionRulesConnector::addLink(DatomsMotionRulesLink *lnk) {
    tabLinks.push_back(lnk);
}

void DatomsMotionRulesLink::addBlockingConnectorsString(const string &str) {
    int n;
    std::size_t found=str.find_first_of(","),prev=0;
    while (found!=std::string::npos) {
        n = stoi(str.substr(prev,found-prev).c_str());
        tabBlockingIDs.push_back(n);
        prev = found+1;
        found=str.find_first_of(",",prev);
    }
    n = stoi(str.substr(prev).c_str());
    tabBlockingIDs.push_back(n);

    // add dest
    tabBlockingIDs.push_back(conTo->ID);
}

void DatomsMotionRulesLink::addBlockingConnector(int n) {
    tabBlockingIDs.push_back(n);
}

bool DatomsMotionRulesLink::isValid(const DatomsBlock *c3d) {
    vector<int>::const_iterator ci = tabBlockingIDs.begin();
// final position (connector) must be free
    if (c3d->getInterface(conTo->ID)->connectedInterface!=NULL) return false;
// all blocking connectors must be free
    while (ci!=tabBlockingIDs.end() && c3d->getInterface(*ci)->connectedInterface==NULL) {
        ci++;
    }
    if (ci!=tabBlockingIDs.end()) OUTPUT << "blocking: "<< *ci << endl;
    return (ci==tabBlockingIDs.end());
}

Cell3DPosition DatomsMotionRulesLink::getFinalPosition(DatomsBlock *c3d) {
    Cell3DPosition finalPosition;
    short orient;
    DatomsBlock *neighbor = (DatomsBlock *)c3d->getInterface(conFrom->ID)->connectedInterface->hostBlock;
    Deformation deformation(c3d,neighbor,axis1,axis2,modelId);
    deformation.init(((DatomsGlBlock*)c3d->ptrGlBlock)->mat);
    deformation.getFinalPositionAndOrientation(finalPosition,orient);
//    OUTPUT << "deformation " << c3d->blockId << ":" << conFrom->ID << " et " << neighbor->blockId << endl;
    return finalPosition;
}

void DatomsMotionRulesLink::sendRotationEvent(DatomsBlock*mobile,DatomsBlock*pivot,double t) {
    Deformation deformation(mobile,pivot,axis1,axis2,modelId);
    getScheduler()->schedule(new DeformationStartEvent(t,mobile,deformation));
}

Deformation DatomsMotionRulesLink::getDeformations(const DatomsBlock* mobile,
                                                   const DatomsBlock* pivot) const {
    return Deformation(mobile,pivot,axis1,axis2,modelId);
}

vector<Cell3DPosition> DatomsMotionRulesLink::getBlockingCellsList(const DatomsBlock *c3d) {
    vector<int>::const_iterator ci = tabBlockingIDs.begin();
    vector<Cell3DPosition> tabPos;
    Cell3DPosition pos;

    //OUTPUT << "getBlockingCellsList ";
    while (ci!=tabBlockingIDs.end()) {
        if (c3d->getNeighborPos(*ci,pos)) {
            //OUTPUT << *ci << ":" << pos << ", ";
            tabPos.push_back(pos);
        }
        ci++;
    }
    //OUTPUT << endl;
    return tabPos;
}

std::ostream& operator<<(std::ostream &stream, DatomsMotionRulesLink const& mrl) {
    stream << mrl.getConFromID() << " -> " << mrl.getConToID();

    switch (mrl.getMRLT()) {
        case HexaFace: stream << " (HEXA)"; break;
        case OctaFace: stream << " (OCTA)"; break;
        default: stream << "(ERR)"; break;
    }

    return stream;
}

} // Datoms namespace
