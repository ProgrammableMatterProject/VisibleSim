#include "ABCcenterCode.h"

Color tabColors[8]={RED,ORANGE,YELLOW,GREEN,CYAN,BLUE,MAGENTA,GREY};
const int delayMin=100;
const int delayDelta=1500;

void ABCcenterCode::startup() {
	addMessageEventFunc(SPINNING_TREE_MSG,_mySpinningTreeFunc);
	addMessageEventFunc(ACK_SP_MSG,_myAckSPFunc);

	console << "start\n";
    srand(time(NULL));
/// initialisation of module code data
    moduleDistance=UINT16_MAX; /// unknown;
    currentDistanceMax=0;
    parent = NULL;
    initTabChildren();
    electedChild=UINT8_MAX; /// unknown

     console << "N:";
    for (size_t i=0; i<nbreNeighborsMax; i++) {
        if (i==electedChild) console << "E ";
        else if (module->getInterface(i)==parent) console << "P ";
        else console << tabChildren[i] << " ";
    }
    console << "\n";

	if (module->blockId==1) { // master id is 1
        setColor(RED);
        moduleDistance=0; /// distance of the master
        sendMessageToAllNeighbors("ATree",new MessageOf<int>(SPINNING_TREE_MSG,moduleDistance),delayMin,delayDelta,0);
	} else {
        setColor(LIGHTGREY);
	}
}

void ABCcenterCode::mySpinningTreeFunc(const MessageOf<uint16_t>*msg, P2PNetworkInterface*sender) {
    uint16_t d = *msg->getData()+1;
    console << "receives SP d=" << d << " from " << sender->getConnectedBlockId() << "\n";
    int nNK=0;
    if (moduleDistance>d) {
        console << "new distance=" << d << "\n";
        moduleDistance = d;
        currentDistanceMax = moduleDistance;
        parent = sender;

        P2PNetworkInterface *p2p;
        for (size_t i=0; i<nbreNeighborsMax; i++) {
            p2p = module->getInterface(i);
            if (tabChildren[i]!=NotConnected && p2p!=parent) {
                tabChildren[i]=StateNotKnown;
                sendMessage("SP",new MessageOf<uint16_t>(SPINNING_TREE_MSG,moduleDistance),p2p,delayMin,delayDelta);
                nNK++;
            }
        }
    } else {
        ackData v(false,moduleDistance,0);
        sendMessage("ACK_0",new MessageOf<ackData>(ACK_SP_MSG,v),sender,delayMin,delayDelta);
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
        if (parent==NULL) {
// initial sender
        } else {
            ackData v(true,moduleDistance,moduleDistance);
            sendMessage("ACK_1",new MessageOf<ackData>(ACK_SP_MSG,v),parent,delayMin,delayDelta);
            setColor(PINK);
        }
    }

};

void ABCcenterCode::myAckSPFunc(const MessageOf<ackData>*msg,P2PNetworkInterface *sender) {
    ackData data = *msg->getData();
    console << "receives ASP d=<" << data.toParent << "," << data.distance << "," << data.distanceMax << "> from " << sender->getConnectedBlockId() << " (" << module->getDirection(sender) << ")\n";
    uint8_t senderDirection = module->getDirection(sender);
    if (data.distanceMax>currentDistanceMax) {
        currentDistanceMax=data.distanceMax;
        electedChild = senderDirection;
    }
    if (data.toParent==0) {
        tabChildren[senderDirection]=(data.distance+1>=moduleDistance)?IsNotChild:StateNotKnown;
    } else {
        tabChildren[senderDirection]=(data.distance>moduleDistance)?IsChild:StateNotKnown;
    }
    int nNK=0;
    for (size_t i=0; i<nbreNeighborsMax; i++) {
        if (tabChildren[i]==StateNotKnown && module->getInterface(i)!=parent) nNK++;
    }
    if (nNK==0) {
        if (parent==NULL) {
// initial sender
        } else {
            ackData v(true,moduleDistance,currentDistanceMax);
            sendMessage("ACK_1",new MessageOf<ackData>(ACK_SP_MSG,v),parent,delayMin,delayDelta);
            setColor(moduleDistance==currentDistanceMax?PINK:LIGHTGREEN);
        }
    }

    console << "N:";
    for (size_t i=0; i<nbreNeighborsMax; i++) {
        if (i==electedChild) console << "E ";
        else if (module->getInterface(i)==parent) console << "P ";
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
	MessageOf<ackData>*msgType = (MessageOf<ackData>*)msg.get();
	cb->myAckSPFunc(msgType,sender);
}
