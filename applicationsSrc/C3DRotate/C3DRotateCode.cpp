#include "C3DRotateCode.h"

double getPotentiel(const Vector3D &pos) {
    static const Vector3D goal(25.0,25.0,17.7,1.0);
    return (pos-goal).norme2();
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

	//tryToMove();
}

bool C3DRotateCode::tryToMove() {
    if (isLocked) return false;
    FCCLattice *lattice = (FCCLattice*)(Catoms3D::getWorld()->lattice);
    double modulePotential = getPotentiel(lattice->gridToWorldPosition(module->position));
	console << "try to move p=" << module->position << ";" << modulePotential << "\n";

    P2PNetworkInterface *p2p;
    Catoms3DBlock *neighbor;
    Catoms3DMotionRules *motionRules = Catoms3D::getWorld()->getMotionRules();
    vector <Catoms3DMotionRulesLink*>vml;

    double bestPotential = 1e32,p;
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
            if (motionRules->validMotionList(module,i,vml)) {
                vector <Catoms3DMotionRulesLink*>::const_iterator ci = vml.begin();
                while (ci!=vml.end()) {
                    v = (*ci)->getFinalPosition(module);
                    if (lattice->isInGrid(v)) {
                        p = getPotentiel(lattice->gridToWorldPosition(v));
                        OUTPUT << (*ci)->getID() << ":" << v << ";" << p << endl;
                        if (p<bestPotential) {
                            bestOri = i;
                            bestPotential=p;
                            bestMRL = (*ci);
                        }
                    }

                    ci++;
                }
            }
        }
    }
    if (bestMRL && bestPotential<modulePotential) {
        console << "Best Orig." << bestOri << " ->" << bestMRL->getConToID() << ":" << bestPotential << "\n";
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
