#include <queue>
#include <climits>
#include "C3DRotateCode.h"

void C3DRotateCode::initDistances() {
    static bool first=true;

    if (first) {
        first=false;

        FCCLattice *lattice = (FCCLattice*)(Catoms3D::getWorld()->lattice);

        short ix,iy,iz;
        Cell3DPosition pos;
        queue<Cell3DPosition> stkCells;
        unsigned int ngoal=0,nwp=0;
        for (iz=0; iz<lattice->gridSize.pt[2]; iz++) {
            for (iy=0; iy<lattice->gridSize.pt[1]; iy++) {
                for (ix=0; ix<lattice->gridSize.pt[0]; ix++) {
                    pos.set(ix,iy,iz);
                    // free cell that must be filled
                    if (target->isInTarget(pos)) {
                        ngoal++;
                        if (!lattice->cellHasBlock(pos)) {
                            lattice->setDistance(pos,0);
                            OUTPUT << "dist0 : " << pos << endl;
                            stkCells.push(pos);
                            nwp++;
                        }
                    }
                }
            }
        }
        OUTPUT << "Target: " << ngoal-nwp << "/" << ngoal << " cells" << endl;
        vector<Cell3DPosition> neighborhood;
        vector <Catoms3DMotionRulesLink*>vml;
        Catoms3DMotionRules *motionRules = (Catoms3D::getWorld())->getMotionRules();
        int currentLevel;
        while (!stkCells.empty()) {
            pos = stkCells.front();
            stkCells.pop();
            currentLevel = lattice->getDistance(pos)+1;
            OUTPUT << pos << " LEVEL " << currentLevel << endl;
            neighborhood = lattice->getNeighborhood(pos);
            vector<Cell3DPosition>::const_iterator ci = neighborhood.begin();
            while (ci!=neighborhood.end()) {
                //OUTPUT << "neighbor " << (*ci) << endl;
                Catoms3DBlock *pivot = (Catoms3DBlock*)lattice->getBlock(*ci);
                if (pivot) {
                    OUTPUT << "catom #" << pivot->blockId <<  endl;
                    for (int i=0; i<12; i++) {
                        //OUTPUT << "connector #" << i << endl;
                        vml.clear();
                        if (motionRules->getValidMotionListFromPivot(pivot,i,vml,lattice,target)) {
                            //OUTPUT << "ans: " << vml.size() << endl;
                            vector <Catoms3DMotionRulesLink*>::const_iterator ct = vml.begin();
                            while (ct!=vml.end()) {
                                Cell3DPosition destPos,fromPos;
                                int to = (*ct)->getConToID();
                                pivot->getNeighborPos(to,destPos);
                                //OUTPUT << "to=" << to << ":" << destPos << endl;
                                if (destPos==pos) {
                                    pivot->getNeighborPos(i,fromPos);
                                    bool full = lattice->cellHasBlock(fromPos);
                                    if (lattice->isInGrid(fromPos) &&
                                        lattice->getDistance(fromPos)>currentLevel ) {
                                        OUTPUT << "dist " << currentLevel << " : "<< fromPos << " " << i << "->" << to << " " << destPos << " full=" << full << endl;
                                        lattice->setDistance(fromPos,currentLevel);
                                        stkCells.push(fromPos);
                                    }
                                }
                                ct++;
                            }
                        }
                    }
                }
                ci++;
            }
        }
    }
}

void C3DRotateCode::startup() {
    isLocked=false;
    currentMotion=NULL;

	addMessageEventFunc(LOCK_MSG,_myLockFunc);
	addMessageEventFunc(ANSLOCK_MSG,_myAnsLockFunc);

    assert(target!=NULL);
    if (target->isInTarget(module->position)) {
        module->setColor(target->getTargetColor(module->position));
    }

    FCCLattice *lattice = (FCCLattice*)(Catoms3D::getWorld()->lattice);
    lattice->initTabDistances();
    initDistances();
	//tryToMove();
}

bool C3DRotateCode::tryToMove() {
    if (isLocked || target->isInTarget(module->position)) return false;
    FCCLattice *lattice = (FCCLattice*)(Catoms3D::getWorld()->lattice);
    unsigned short moduleDistance = lattice->getDistance(module->position);

	console << "try to move p=" << module->position << ";" << moduleDistance << "\n";

    P2PNetworkInterface *p2p;
    Catoms3DBlock *neighbor;
    Catoms3DMotionRules *motionRules = Catoms3D::getWorld()->getMotionRules();
    vector <Catoms3DMotionRulesLink*>vml;

    unsigned short bestDistance = USHRT_MAX,p;
    int bestOri = -1;
    Catoms3DMotionRulesLink* bestMRL=NULL;
    Cell3DPosition v;
    for (int i=0; i<12; i++) {
        p2p=module->getInterface(i);
        if (p2p && p2p->connectedInterface) {
            neighbor = (Catoms3DBlock *)p2p->connectedInterface->hostBlock;
            console << "interface#" << i << " to " << neighbor->blockId << "#" << neighbor->getDirection(p2p->connectedInterface) << "\n";
            // liste des mouvements possibles
            vml.clear();
            if (motionRules->getValidMotionList(module,i,vml)) {
                vector <Catoms3DMotionRulesLink*>::const_iterator ci = vml.begin();
                while (ci!=vml.end()) {
                    v = (*ci)->getFinalPosition(module);
                    if (lattice->isInGrid(v)) {
                        p = lattice->getDistance(v);
                        OUTPUT << (*ci)->getID() << ":" << v << ";" << p << endl;
                        if (p<bestDistance) {
                            bestOri = i;
                            bestDistance=p;
                            bestMRL = (*ci);
                        }
                    }

                    ci++;
                }
            }
        }
    }
    if (bestMRL && bestDistance<moduleDistance) {
        console << "Best Orig." << bestOri << " ->" << bestMRL->getConToID() << ":" << bestDistance << "\n";
        p2p=module->getInterface(bestOri);
        currentMotion = new Motions(module,(Catoms3DBlock *)p2p->connectedInterface->hostBlock,bestMRL);
        console << "send LOCK() to " << p2p->connectedInterface->hostBlock->blockId << "\n";
        sendMessage(new MessageOf<Motions>(LOCK_MSG,*currentMotion),p2p,100,200);
        return true;
    }
    return false;
}

void C3DRotateCode::myLockFunc(const MessageOf<Motions>*msg, P2PNetworkInterface*sender) {
	Motions msgData = *msg->getData();
    bool answer = false;
    if (!isLocked) {
        FCCLattice *FCClat = (FCCLattice*)(Catoms3D::getWorld()->lattice);
        answer=true;
        int n=0;
        vector <Cell3DPosition>::const_iterator ci = msgData.tabCells.begin();
        while (answer && ci!=msgData.tabCells.end()) {
            answer = FCClat->lockCell(*ci);
            OUTPUT << "lock " << *ci << " : " << answer << endl;
            ci++;
            n++;
        }
        OUTPUT << "isok=" << answer << endl;
        // if there is an already locked cell, we must unlock the previous
        if (!answer) {
            ci--;
            while(n--) {
                OUTPUT << "unlock " << *ci << endl;
                FCClat->unlockCell(*ci);
                ci--;
            }
        }
        isLocked = answer;
        if (isLocked) module->setColor(RED);
    }
    console << "send ANSLOCK(" << answer << ") to " << sender->connectedInterface->hostBlock->blockId << "\n";
    sendMessage(new MessageOf<bool>(ANSLOCK_MSG,answer),sender,100,200);
};

void C3DRotateCode::myAnsLockFunc(const MessageOf<bool>*msg, P2PNetworkInterface*sender) {
	bool msgData = *msg->getData();

    if (msgData) {
        currentMotion->MRlist->sendRotationEvent(module,currentMotion->fixed,scheduler->now()+2000);
    }
};

void C3DRotateCode::onMotionEnd() {
// unlock cells

    FCCLattice *FCClat = (FCCLattice*)(Catoms3D::getWorld()->lattice);
    vector <Cell3DPosition>::const_iterator ci = currentMotion->tabCells.begin();
    while (ci!=currentMotion->tabCells.end()) {
        OUTPUT << "unlock " << *ci << endl;
        FCClat->unlockCell(*ci);
        ci++;
    }
    delete currentMotion;
    isLocked = false;
    module->setColor(YELLOW);
    currentMotion=NULL;
}

void C3DRotateCode::onTap(int n) {
    cout << "tap" << n << endl;
    tryToMove();
}

void _myLockFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	C3DRotateCode *cb = (C3DRotateCode*)codebloc;
	MessageOf<Motions>*msgType = (MessageOf<Motions>*)msg.get();
	cb->myLockFunc(msgType,sender);
}

void _myAnsLockFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	C3DRotateCode *cb = (C3DRotateCode*)codebloc;
	MessageOf<bool>*msgType = (MessageOf<bool>*)msg.get();
	cb->myAnsLockFunc(msgType,sender);
}

Motions::Motions(Catoms3DBlock *m,Catoms3DBlock *f,Catoms3DMotionRulesLink* mrl) {
    mobile = m;
    fixed = f;
    MRlist = mrl;
    tabCells = mrl->getBlockingCellsList(mobile);
}

Motions::~Motions() {
    tabCells.clear();
}
