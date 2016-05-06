#include "ABCcenterCode.h"

Color tabColors[8]={RED,ORANGE,YELLOW,GREEN,CYAN,BLUE,MAGENTA,GREY};
const int delayMin=100;
const int delayDelta=1500;

void ABCcenterCode::startup() {
	addMessageEventFunc(SPINNING_TREE_MSG,_mySpinningTreeFunc);
	addMessageEventFunc(ACK_SP_MSG,_myAckSPFunc);

	console << "start\n";
    srand(time(NULL));
    // initialize children array
    initTabChildren();

    console << "N:";
    for (size_t i=0; i<nbreNeighborsMax; i++) {
        console << tabChildren[i] << " ";
    }
    console << "\n";

	if (module->blockId==1) { // master id is 1
        setColor(RED);
        distance=0;
        sendMessageToAllNeighbors("ATree",new MessageOf<int>(SPINNING_TREE_MSG,distance),delayMin,delayDelta,0);
	} else {
        distance=UINT16_MAX; // unknown
        setColor(LIGHTGREY);
	}
}

void ABCcenterCode::mySpinningTreeFunc(const MessageOf<uint16_t>*msg, P2PNetworkInterface*sender) {
    uint16_t d = *msg->getData()+1;
    console << "receives SP d=" << d << " from " << sender->getConnectedBlockId() << "\n";
    int nNK=0;
    if (distance>d) {
        console << "new distance=" << d << "\n";
        distance = d;
        parent = sender;

        P2PNetworkInterface *p2p;
        for (size_t i=0; i<nbreNeighborsMax; i++) {
            p2p = module->getInterface(i);
            if (tabChildren[i]!=NotConnected && p2p!=parent) {
                tabChildren[i]=StateNotKnown;
                sendMessage("SP",new MessageOf<uint16_t>(SPINNING_TREE_MSG,distance),p2p,delayMin,delayDelta);
                nNK++;
            }
        }
    } else {
        sendMessage("ACK_0",new MessageOf<uint16_t>(ACK_SP_MSG,0),sender,delayMin,delayDelta);
        tabChildren[module->getDirection(sender)]=IsNotChild;

        for (size_t i=0; i<nbreNeighborsMax; i++) {
            if (tabChildren[i]==StateNotKnown && module->getInterface(i)!=parent) nNK++;
//            else if (tabChildren[i]==IsChild) nCh++;
        }
    }
//    setColor(tabColors[distance%8]);
    console << "N:";
    for (size_t i=0; i<nbreNeighborsMax; i++) {
        if (module->getInterface(i)==parent) console << "P ";
        else console << tabChildren[i] << " ";
    }
    console << "\n";

    if (nNK==0) {
        sendMessage("ACK_1",new MessageOf<uint8_t>(ACK_SP_MSG,distance),parent,delayMin,delayDelta);
//        setColor(nCh==0?PINK:tabColors[distance%8]);
    }

};

void ABCcenterCode::myAckSPFunc(const MessageOf<uint16_t>*msg,P2PNetworkInterface *sender) {
    uint16_t d = *msg->getData();
    console << "receives ASP d=" << d << " from " << sender->getConnectedBlockId() << " (" << module->getDirection(sender) << ")\n";
    if (d==0) {
        tabChildren[module->getDirection(sender)]=IsNotChild;
    } else {
        tabChildren[module->getDirection(sender)]=(d>distance)?IsChild:StateNotKnown;
    }
    int nNK=0;
    for (size_t i=0; i<nbreNeighborsMax; i++) {
        if (tabChildren[i]==StateNotKnown && module->getInterface(i)!=parent) nNK++;
//        else if (tabChildren[i]==IsChild) nCh++;
    }
    if (nNK==0) {
        sendMessage("ACK_1",new MessageOf<uint8_t>(ACK_SP_MSG,distance),parent,delayMin,delayDelta);
//        setColor(nCh==0?PINK:tabColors[distance%8]);
    }

    console << "N:";
    for (size_t i=0; i<nbreNeighborsMax; i++) {
        if (module->getInterface(i)==parent) console << "P ";
        else console << tabChildren[i] << " ";
    }
    console << "\n";

}

void ABCcenterCode::initTabChildren() {
    for (size_t i=0; i<nbreNeighborsMax; i++) {
        tabChildren[i]=(module->getInterface(i)->connectedInterface==NULL?NotConnected:StateNotKnown);
    }
}

void _mySpinningTreeFunc(GenericCodeBlock *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	ABCcenterCode *cb = (ABCcenterCode*)codebloc;
	MessageOf<uint16_t>*msgType = (MessageOf<uint16_t>*)msg.get();
	cb->mySpinningTreeFunc(msgType,sender);
}

void _myAckSPFunc(GenericCodeBlock *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	ABCcenterCode *cb = (ABCcenterCode*)codebloc;
	MessageOf<uint16_t>*msgType = (MessageOf<uint16_t>*)msg.get();
	cb->myAckSPFunc(msgType,sender);
}
