#include "datomsMotionRules.h"
#include "deformationEvents.h"
#include "datomsWorld.h"
#include "datomsMotionEngine.h"

using namespace std;
using namespace BaseSimulator::utils;

//! \namespace Datoms
namespace Datoms {


const vector<DatomsMotionRulesLink*>&
DatomsMotionRules::getMotionRulesLinksForConnector(short con) {
    if (con < 0 || con > 11)
        throw InvalidArgumentException(__PRETTY_FUNCTION__,
                                       std::string("con = ") + to_string(con));

    return tabConnectors[con]->tabLinks;
}


DatomsMotionRules::DatomsMotionRules() {
    const Vector3D up(0,0,1),xy(1,1,0),_xy(-1,1,0);
    Lattice *lattice = DatomsWorld::getWorld()->lattice;
    float r = 0.5*lattice->gridScale[0],
    c = 0.30389*r, // c/2
    cx = 0.56374*r,
    cy = 0.12774*r, // e/sqrt(2)
    cz = 0.92516932*r, // c(0.5+sqrt(2)/2)+e
    dx = 0.872258*r, // dy+c/sqrt(2)
    dy = 0.436129*r, // c/2+e/sqrt(2)
    dz = 0.48904*r; // c/2+e

// allocation of connectors
    for (int i=0; i<12; i++) {
        tabConnectors[i] = new DatomsMotionRulesConnector(i);
        tabConnectors[i]->tabPtrPistons[0]=NULL;
        tabConnectors[i]->tabPtrPistons[1]=NULL;
        tabConnectors[i]->tabPtrPistons[2]=NULL;
        tabConnectors[i]->tabPtrPistons[3]=NULL;
    }

    tabPistons[0] = new DatomsMotionRulesPiston(0,Vector3D(r,r,0),Piston012A);
    tabPistons[0]->setAxis(0,Vector3D(r,-c,-c),up); //#0
    tabPistons[0]->setAxis(1,Vector3D(-c,r,c),-up); //#1
    tabPistons[0]->setAxis(2,Vector3D(cx,cy,cz),_xy); //#2
    tabPistons[0]->setAxis(3,Vector3D(cy,cx,-cz),-_xy); //#10
    tabPistons[1] = new DatomsMotionRulesPiston(1,Vector3D(-r,r,0),Piston136B);
    tabPistons[1]->setAxis(0,Vector3D(c,r,-c),up); //#1
    tabPistons[1]->setAxis(1,Vector3D(-cy,cx,cz),-xy); //#3
    tabPistons[1]->setAxis(2,Vector3D(-r,-c,c),-up); //#6
    tabPistons[1]->setAxis(3,Vector3D(-cx,cy,-cz),xy); //#11
    tabPistons[2] = new DatomsMotionRulesPiston(2,Vector3D(-r,-r,0),Piston4678);
    tabPistons[2]->setAxis(0,Vector3D(-cx,-cy,cz),-_xy); //#4
    tabPistons[2]->setAxis(1,Vector3D(-r,c,-c),up); //#6
    tabPistons[2]->setAxis(2,Vector3D(c,-r,c),-up); //#7
    tabPistons[2]->setAxis(3,Vector3D(-cy,-cx,-cz),_xy); //#8
    tabPistons[3] = new DatomsMotionRulesPiston(3,Vector3D(r,-r,0),Piston0579);
    tabPistons[3]->setAxis(0,Vector3D(r,c,c),-up); //#0
    tabPistons[3]->setAxis(1,Vector3D(cy,-cx,cz),xy); //#5
    tabPistons[3]->setAxis(2,Vector3D(-c,-r,-c),up); //#7
    tabPistons[3]->setAxis(3,Vector3D(cx,-cy,-cz),-xy); //#9
    tabPistons[4] = new DatomsMotionRulesPiston(4,Vector3D(0,0,M_SQRT2*r),Piston2345);
    tabPistons[4]->setAxis(0,Vector3D(dx,dy,dz),-_xy); //#2
    tabPistons[4]->setAxis(1,Vector3D(-dy,dx,dz),xy); //#3
    tabPistons[4]->setAxis(2,Vector3D(-dx,-dy,dz),_xy); //#4
    tabPistons[4]->setAxis(3,Vector3D(dy,-dx,dz),-xy); //#5
    tabPistons[5] = new DatomsMotionRulesPiston(5,Vector3D(0,0,-M_SQRT2*r),Piston89AB);
    tabPistons[5]->setAxis(0,Vector3D(-dx,-dy,-dz),-_xy); //#8
    tabPistons[5]->setAxis(1,Vector3D(dy,-dx,-dz),xy); //#9
    tabPistons[5]->setAxis(2,Vector3D(dx,dy,-dz),_xy); //#10
    tabPistons[5]->setAxis(3,Vector3D(-dy,dx,-dz),-xy); //#11

    addLinks(0,tabPistons[0],2,1,10,tabPistons[4],tabPistons[1],tabPistons[5],0,2,1,3);
    addLinks(0,tabPistons[3],9,7,5,tabPistons[5],tabPistons[1],tabPistons[4],0,3,2,1);
    addLinks(1,tabPistons[1],3,6,11,tabPistons[4],tabPistons[2],tabPistons[5],0,1,2,3);
    addLinks(1,tabPistons[0],10,0,2,tabPistons[5],tabPistons[3],tabPistons[4],1,3,0,2);
    addLinks(2,tabPistons[4],5,4,3,tabPistons[3],tabPistons[2],tabPistons[1],0,3,2,1);
    addLinks(2,tabPistons[0],1,10,0,tabPistons[1],tabPistons[5],tabPistons[3],2,1,3,0);
    addLinks(3,tabPistons[1],6,11,1,tabPistons[2],tabPistons[5],tabPistons[0],1,2,3,0);
    addLinks(3,tabPistons[4],2,5,4,tabPistons[0],tabPistons[3],tabPistons[2],1,0,3,2);
    addLinks(4,tabPistons[2],7,8,6,tabPistons[3],tabPistons[5],tabPistons[1],0,2,3,1);
    addLinks(4,tabPistons[4],3,2,5,tabPistons[1],tabPistons[0],tabPistons[3],2,1,0,3);
    addLinks(5,tabPistons[3],0,9,7,tabPistons[0],tabPistons[5],tabPistons[2],1,0,3,2);
    addLinks(5,tabPistons[4],4,3,2,tabPistons[2],tabPistons[1],tabPistons[0],3,2,1,0);
    addLinks(6,tabPistons[2],4,7,8,tabPistons[4],tabPistons[3],tabPistons[5],1,0,2,3);
    addLinks(6,tabPistons[1],11,1,3,tabPistons[5],tabPistons[0],tabPistons[4],2,3,0,1);
    addLinks(7,tabPistons[3],5,0,9,tabPistons[4],tabPistons[0],tabPistons[5],2,1,0,3);
    addLinks(7,tabPistons[2],8,6,4,tabPistons[5],tabPistons[1],tabPistons[4],2,3,1,0);
    addLinks(8,tabPistons[5],9,10,11,tabPistons[3],tabPistons[0],tabPistons[1],0,1,2,3);
    addLinks(8,tabPistons[2],6,4,7,tabPistons[1],tabPistons[4],tabPistons[3],3,1,0,2);
    addLinks(9,tabPistons[3],7,5,0,tabPistons[2],tabPistons[4],tabPistons[0],3,2,1,0);
    addLinks(9,tabPistons[5],10,11,8,tabPistons[0],tabPistons[1],tabPistons[2],1,2,3,0);
    addLinks(10,tabPistons[0],0,2,1,tabPistons[3],tabPistons[4],tabPistons[1],3,0,2,1);
    addLinks(10,tabPistons[5],11,8,9,tabPistons[1],tabPistons[2],tabPistons[3],2,3,0,1);
    addLinks(11,tabPistons[1],1,3,6,tabPistons[0],tabPistons[4],tabPistons[2],3,0,1,2);
    addLinks(11,tabPistons[5],8,9,10,tabPistons[2],tabPistons[3],tabPistons[0],3,0,1,2);
}

DatomsMotionRules::~DatomsMotionRules() {
    for (int i=0; i<12; i++) {
        delete tabConnectors[i];
    }
    for (int i=0; i<6; i++) {
        delete tabPistons[i];
    }
}

bool DatomsMotionRulesLink::concernsConnector(short conId) const {
    return getConnectors()[0] == conId || getConnectors()[1] == conId;
}

bool DatomsMotionRulesLink::concernsConnectors(short conId1, short conId2) const {
    return concernsConnector(conId1) && concernsConnector(conId2);
}

std::array<short, 2> DatomsMotionRulesLink::getConnectors() const {
    return {(short)conFrom->ID, (short)conTo->ID};
}

string DatomsMotionRulesLink::getID() {
    string c="X->PP->X";
    uint8_t id1 = conFrom->ID;
    uint8_t id2 = conTo->ID;
    char ch = (id1<10?'0'+id1:'A'+id1-10);
    c[0]=ch;
    ch = (id2<10?'0'+id2:'A'+id2-10);
    c[7]=ch;
    c[4] = '0'+piston->ID;
    return c;
}

void DatomsMotionRules::addLinks(int conFrom, DatomsMotionRulesPiston* act, int id1, int id2, int id3, const DatomsMotionRulesPiston* Pleft, const DatomsMotionRulesPiston* Pfront, const DatomsMotionRulesPiston* Pright,short j0,short j1,short j2,short j3) {

    tabConnectors[conFrom]->addPiston(act);
    DatomsMotionRulesLink *lnk = new DatomsMotionRulesLink(tabConnectors[conFrom],tabConnectors[id1],act,j0,j1); // go to left
    tabConnectors[conFrom]->tabLinks.push_back(lnk);
    lnk->addBlockingDirection(act->direction+Pleft->direction); // left must be free (goal)
    lnk->addBlockingDirection(act->direction+Pright->direction,Pleft->direction); // right must be free or compressed along Pleft direction
    lnk->addBlockingDirection(act->direction+Pfront->direction,-Pfront->direction); // front must be free or compressed along -Pfront direction
    lnk->addBlockingDirection(Pleft->direction+2.0*act->direction-Pfront->direction);
    lnk->addBlockingDirection(Pleft->direction+2.0*act->direction+Pfront->direction,-Pfront->direction);
    lnk->addBlockingDirection(Pright->direction+2.0*act->direction-Pfront->direction,Pleft->direction);
    lnk->addBlockingDirection(2.0*act->direction);
    /*Pleft->direction+2.0*act->direction-Pfront->direction,
        Pleft->direction+2.0*act->direction+Pfront->direction,
        Pright->direction+2.0*act->direction-Pfront->direction,*/

    lnk = new DatomsMotionRulesLink(tabConnectors[conFrom],tabConnectors[id2],act,j0,j2); // go ahead
    tabConnectors[conFrom]->tabLinks.push_back(lnk);
    lnk->addBlockingDirection(act->direction+Pleft->direction,Pright->direction); // left must be free or compressed along Pright direction
    lnk->addBlockingDirection(act->direction+Pright->direction,Pleft->direction); // right must be free or compressed along Pleft direction
    lnk->addBlockingDirection(act->direction+Pfront->direction); // front must be free (goal)
    lnk->addBlockingDirection(Pleft->direction+2.0*act->direction-Pfront->direction,Pright->direction);
    lnk->addBlockingDirection(Pleft->direction+2.0*act->direction+Pfront->direction,Pright->direction);
    lnk->addBlockingDirection(Pright->direction+2.0*act->direction-Pfront->direction,Pleft->direction);
    lnk->addBlockingDirection(Pright->direction+2.0*act->direction+Pfront->direction,Pleft->direction);
    lnk->addBlockingDirection(2.0*act->direction);

    /*	Pleft->direction+2.0*act->direction-Pfront->direction,
        Pright->direction+2.0*act->direction-Pfront->direction,
        Pleft->direction+2.0*act->direction+Pfront->direction,
        Pright->direction+2.0*act->direction+Pfront->direction,*/

    lnk = new DatomsMotionRulesLink(tabConnectors[conFrom],tabConnectors[id3],act,j0,j3); // go to right
    tabConnectors[conFrom]->tabLinks.push_back(lnk);
    /*addLink(conFrom,id3,act,
        Pright->direction+2.0*act->direction-Pfront->direction,
        Pleft->direction+2.0*act->direction-Pfront->direction,
        Pright->direction+2.0*act->direction-Pfront->direction,
        j0,j3);*/
    lnk->addBlockingDirection(act->direction+Pleft->direction,Pright->direction); // left must be free or compressed along Pright direction
    lnk->addBlockingDirection(act->direction+Pright->direction); // right must be free
    lnk->addBlockingDirection(act->direction+Pfront->direction,-Pfront->direction); // front must be free or compressed along -Pfront direction
    lnk->addBlockingDirection(Pleft->direction+2.0*act->direction-Pfront->direction,Pright->direction);
    lnk->addBlockingDirection(Pright->direction+2.0*act->direction+Pfront->direction,-Pfront->direction);
    lnk->addBlockingDirection(Pright->direction+2.0*act->direction-Pfront->direction);
    lnk->addBlockingDirection(2.0*act->direction);
}

bool DatomsMotionRules::getValidMotionList(const DatomsBlock* pivot,int from,vector<DatomsMotionRulesLink*>&vec) {
    DatomsMotionRulesConnector *conn = tabConnectors[from];
    bool notEmpty=false;

    for (DatomsMotionRulesLink* ptrl:conn->tabLinks) {
        //OUTPUT << "#" << pivot->blockId << ":" << from << " -> " << ptrl->getConToID();
        if (ptrl->isValid(pivot)) {
            vec.push_back(ptrl);
            notEmpty=true;
            //OUTPUT << " valid P" << ptrl->piston->ID;
        }
        //OUTPUT << endl;
    }
    return notEmpty;
}

DatomsMotionRulesLink::DatomsMotionRulesLink(DatomsMotionRulesConnector *from,DatomsMotionRulesConnector *to,DatomsMotionRulesPiston *p,short s0,short s1) {
    conFrom = from;
    conTo = to;
    piston = p;
    jointFrom=s0; // for the rotation axis
    jointTo=s1; // for the rotation axis
}

void DatomsMotionRulesLink::addBlockingDirection(const Vector3D &dir) {
    tabBlockingCellDirections.push_back(BlockingCell(dir));
}

void DatomsMotionRulesLink::addBlockingDirection(const Vector3D &dir, const Vector3D &compDir) {
    tabBlockingCellDirections.push_back(BlockingCell(dir,compDir));
}

bool DatomsMotionRulesLink::isValid(const DatomsBlock *pivot) {
    Lattice *lattice = DatomsWorld::getWorld()->lattice;

// cell of destination position must be in the lattice
    Cell3DPosition pos;
    pivot->getNeighborPos(conTo->ID,pos);
    if (!lattice->isInGrid(pos)) {
        //OUTPUT << " dest pos: " << pos << " is not in the grid" << endl;
        return false;
    }
    vector<pair<Cell3DPosition,Vector3D>> blocking = getBlockingCellsList(pivot);
    for (pair<Cell3DPosition,Vector3D> p:blocking) {
        if (p.second[3]==1 && !lattice->isFree(p.first)) {
            //OUTPUT << " pos: " << p.first << " is not free" << endl;
            return false;
        }
        // TODO vérifier que les modules en tabBlockingCells ne sont pas attachés que par des connecteurs attachés au piston

    }
    return true;
}

vector<pair<Cell3DPosition,Vector3D>> DatomsMotionRulesLink::getBlockingCellsList(const DatomsBlock *pivot) {
    Lattice *lattice = DatomsWorld::getWorld()->lattice;
    vector<pair<Cell3DPosition,Vector3D>> tabPos;

    Vector3D Vm,Vcomp;

    for (const BlockingCell bc:tabBlockingCellDirections) {
        Vm = bc.relPos;
        Vm.setPoint(true);
        Vm = pivot->getGlBlock()->mat*Vm;
        if (!bc.mustBeFree) {
            Vcomp = pivot->getGlBlock()->mat*bc.compDir;
        } else {
            Vcomp.set(0,0,0,1);
        }
        tabPos.push_back(make_pair(lattice->worldToGridPosition(Vm),Vcomp));
    }
    return tabPos;
}

vector<pair<DatomsBlock*,PistonId>> DatomsMotionRulesLink::getBlockingDatoms(const DatomsBlock *pivot) {
    Lattice *lattice = DatomsWorld::getWorld()->lattice;
    vector<pair<DatomsBlock*,PistonId>> tabPos;

    //OUTPUT << "getBlockingModulesList " << endl;
    Vector3D Vm,Vcomp;

    for (const BlockingCell bc:tabBlockingCellDirections) {
        Vm = bc.relPos;
        Vm.setPoint(true);
        Vm = pivot->getGlBlock()->mat*Vm;
        if (!bc.mustBeFree) {
            // get module
            DatomsBlock *datom = (DatomsBlock*)lattice->getBlock(lattice->worldToGridPosition(Vm));
            if (datom) {
                Vcomp = bc.compDir;
                Vcomp.setPoint(true);
                Vcomp = pivot->getGlBlock()->mat * Vcomp;
                //OUTPUT << "search pid" << Vcomp << endl;
                PistonId pid =  ((DatomsWorld*) getWorld())->getMotionRules()->getPistonId(datom,Vcomp);
                tabPos.push_back(make_pair(datom,pid));
                //OUTPUT << "Blocking Datom #" << datom->blockId << ":" << pid << endl;
            }
        }
    }
    //OUTPUT << endl;
    return tabPos;

}

Cell3DPosition DatomsMotionRulesLink::getFinalPosition(DatomsBlock *mobile) {
    Cell3DPosition finalPosition;
    short orient;
    DatomsBlock *pivot = (DatomsBlock *)mobile->getInterface(conFrom->ID)->connectedInterface->hostBlock;
    Vector3D P(piston->direction[0],piston->direction[1],piston->direction[2],1);
    Vector3D posPiston = pivot->getGlBlock()->mat*P;
    PistonId mobileId=((DatomsWorld*) getWorld())->getMotionRules()->getPistonId(mobile,posPiston);
    vector<pair<DatomsBlock*,PistonId>> blockingModules;
    Deformation deformation(mobile,pivot,piston->Caxis[jointFrom],piston->Vaxis[jointFrom],piston->Caxis[jointTo],piston->Vaxis[jointTo],mobileId,piston->modelId,blockingModules);
    deformation.init();
    deformation.getFinalPositionAndOrientation(finalPosition,orient);
    OUTPUT << "deformation " << mobile->blockId << ":P" << piston->ID << ":" << conFrom->ID << " / " << pivot->blockId << endl;
    return finalPosition;
}

void DatomsMotionRulesLink::sendRotationEvent(DatomsBlock*mobile,DatomsBlock*pivot,double t) {
  /*  Deformation deformation(mobile,pivot,axis1,axis2,modelId);
    getScheduler()->schedule(new DeformationStartEvent(t,mobile,deformation));*/
}

Deformation DatomsMotionRulesLink::getDeformations(const DatomsBlock* mobile,const DatomsBlock* pivot, vector<pair<DatomsBlock*,PistonId>> blockingModules) const {
    Vector3D P(piston->direction[0],piston->direction[1],piston->direction[2],1);
    Vector3D posPiston = pivot->getGlBlock()->mat*P;
    PistonId mobileId=((DatomsWorld*) getWorld())->getMotionRules()->getPistonId(mobile,posPiston);

    return Deformation(mobile,pivot,piston->Caxis[jointFrom],piston->Vaxis[jointFrom],piston->Caxis[jointTo],piston->Vaxis[jointTo],mobileId,piston->modelId,blockingModules);
}

PistonId DatomsMotionRules::getPistonId(const DatomsBlock *module,const Vector3D &pos) {
    Vector3D P;
    Vector3D vecPiston;

    DatomsMotionRulesPiston *pmin;
    double dmin=1e32,d2;
    for (DatomsMotionRulesPiston *p:tabPistons) {
        P.set(p->direction[0],p->direction[1],p->direction[2],1);
        vecPiston = module->getGlBlock()->mat*P-pos;
        d2 = vecPiston.norme2();
        if (d2<dmin) {
            pmin=p;
            dmin = d2;
        }
    }
    return pmin->modelId;
}

std::ostream& operator<<(std::ostream &stream, DatomsMotionRulesLink const& mrl) {
    std::array<short, 2> connectors = mrl.getConnectors();
    stream << connectors[0] << " -> " << connectors[1];

    return stream;
}

void DatomsMotionRulesConnector::addPiston(DatomsMotionRulesPiston* ptr) {
    DatomsMotionRulesPiston** tab = tabPtrPistons;
    while (*tab!=NULL) {
        tab++;
    }
    *tab=ptr;
}

} // Datoms namespace
