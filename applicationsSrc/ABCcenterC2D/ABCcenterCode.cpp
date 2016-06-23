#include "ABCcenterCode.h"

//#define verbose

Color myTabColors[8]={RED,ORANGE,YELLOW,GREEN,CYAN,BLUE,MAGENTA,GREY};
const int delayMin=100;
const int delayDelta=20;

void ABCcenterCode::startup() {
	addMessageEventFunc(SPINNING_TREE_MSG,_mySpinningTreeFunc);
	addMessageEventFunc(ACK_SP_MSG,_myAckSPFunc);
	addMessageEventFunc(GO2B_MSG,_myGo2BFunc);

	console << "start\n";
    srand(time(NULL));
/// initialisation of module code data
    initModuleData();
    currentStep=0;
    distance2ABC[0]=UINT16_MAX;
    distance2ABC[1]=UINT16_MAX;
    distance2ABC[2]=UINT16_MAX;

#ifdef verbose
    for (size_t i=0; i<nbreNeighborsMax; i++) {
        if (i==electedChild) console << "E ";
        else if (module->getInterface(i)==parent) console << "P ";
        else console << tabChildren[i] << " ";
    }
    console << "\n";
#endif
	if (module->blockId==1) { // master id is 1
        setColor(RED);
        moduleDistance=0; /// distance of the master
        distance2ABC[0]=0;
        currentStep=1;
        SPdata sp(moduleDistance,1);
#ifdef verbose
        sendMessageToAllNeighbors("SP",new MessageOf<SPdata>(SPINNING_TREE_MSG,sp),delayMin,delayDelta,0);
#else
        sendMessageToAllNeighbors(new MessageOf<SPdata>(SPINNING_TREE_MSG,sp),delayMin,delayDelta,0);
#endif
	} else {
        setColor(LIGHTGREY);
	}
}

void ABCcenterCode::mySpinningTreeFunc(const MessageOf<SPdata>*msg, P2PNetworkInterface*sender) {
    SPdata data = *msg->getData();
    uint16_t d = data.distance+1;
#ifdef verbose
    console << "receives SP <" << d << "," << (int)(data.step) << "> from " << sender->getConnectedBlockId() << "\n";
#endif
    int nNK=0;
    if (data.step!=currentStep) {
        initModuleData();
        currentStep = data.step;
    }
    if (d<moduleDistance) {
#ifdef verbose
        console << "step " << (int)(currentStep) << " distance=" << d << "\n";
#endif
        P2PNetworkInterface *p2p;
        for (size_t i=0; i<nbreNeighborsMax; i++) {
            p2p = module->getInterface(i);
            if (tabChildren[i]!=NotConnected && p2p!=parent && p2p!=sender) {
                tabChildren[i]=StateNotKnown;
                SPdata sp(d,currentStep);
#ifdef verbose
                sendMessage("SP",new MessageOf<SPdata>(SPINNING_TREE_MSG,sp),p2p,delayMin,delayDelta);
#else
                sendMessage(new MessageOf<SPdata>(SPINNING_TREE_MSG,sp),p2p,delayMin,delayDelta);
#endif
                nNK++;
            }
        }
        if (parent) {
            uint8_t parentDirection = module->getDirection(parent);
            if (d<moduleDistance-2) {

                SPdata sp(d,currentStep);
#ifdef verbose
                sendMessage("SP",new MessageOf<SPdata>(SPINNING_TREE_MSG,sp),parent,delayMin,delayDelta);
#else
                sendMessage(new MessageOf<SPdata>(SPINNING_TREE_MSG,sp),parent,delayMin,delayDelta);
#endif
                tabChildren[parentDirection]=StateNotKnown;
            } else {
                ackData v(false,d,0,0);
#ifdef verbose
                sendMessage("ACK_0",new MessageOf<ackData>(ACK_SP_MSG,v),parent,delayMin,delayDelta);
#else
                sendMessage(new MessageOf<ackData>(ACK_SP_MSG,v),parent,delayMin,delayDelta);
#endif // verbose
                tabChildren[parentDirection]=IsNotChild;
            }
        }
        parent = sender;
        moduleDistance = d;
        distance2ABC[currentStep-1]=d;
        currentDistanceMax = moduleDistance;
        currentIdMax = module->blockId;
        if (nNK==0) {
            if (parent==NULL) {
    // initial sender
#ifdef verbose
                char str[10];
                sprintf(str,"GO-%d",currentStep);
                sendMessage(str,new MessageOf<uint8_t>(GO2B_MSG,currentStep),module->getInterface(electedChild),delayMin,delayDelta);
#else
                sendMessage(new MessageOf<uint8_t>(GO2B_MSG,currentStep),module->getInterface(electedChild),delayMin,delayDelta);
#endif // verbose
            } else {
                ackData v(true,currentDistanceMax,currentDistanceMax,currentIdMax);
#ifdef verbose
                sendMessage("ACK_1",new MessageOf<ackData>(ACK_SP_MSG,v),parent,delayMin,delayDelta);
#else
                sendMessage(new MessageOf<ackData>(ACK_SP_MSG,v),parent,delayMin,delayDelta);
#endif // verbose
            }
        }

    } else {
        ackData v(false,moduleDistance,0,0);
#ifdef verbose
        sendMessage("ACK_0",new MessageOf<ackData>(ACK_SP_MSG,v),sender,delayMin,delayDelta);
#else
        sendMessage(new MessageOf<ackData>(ACK_SP_MSG,v),sender,delayMin,delayDelta);
#endif // verbose
    }
#ifdef verbose
    console << "N:";
    for (size_t i=0; i<nbreNeighborsMax; i++) {
        if (i==electedChild) console << "E ";
        else if (module->getInterface(i)==parent) console << "P ";
        else console << tabChildren[i] << " ";
    }
    console << "\n";
#endif // verbose
};

void ABCcenterCode::myAckSPFunc(const MessageOf<ackData>*msg,P2PNetworkInterface *sender) {
    ackData data = *msg->getData();
#ifdef verbose
    console << "receives ACK d=<" << data.toParent << "," << data.distance << "," << data.distanceMax << "," << data.idMax << "> from " << sender->getConnectedBlockId() << " (" << module->getDirection(sender) << ")\n";
#endif // verbose
    uint8_t senderDirection = module->getDirection(sender);
    if (data.distanceMax>currentDistanceMax || (data.distanceMax==currentDistanceMax && data.idMax>currentIdMax)) {
        currentDistanceMax = data.distanceMax;
        currentIdMax = data.idMax;
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
            if (currentStep<3) {
// initial sender
#ifdef verbose
                char str[10];
                sprintf(str,"GO-%d",currentStep);
                sendMessage(str,new MessageOf<uint8_t>(GO2B_MSG,currentStep),module->getInterface(electedChild),delayMin,delayDelta);
#else
                sendMessage(new MessageOf<uint8_t>(GO2B_MSG,currentStep),module->getInterface(electedChild),delayMin,delayDelta);
#endif // verbose
            } else {
                console << "Activation <" << data.toParent << "," << data.distance << "," << data.distanceMax << "," << data.idMax << ">\n";
            }
        } else {
            ackData v(true,moduleDistance,currentDistanceMax,currentIdMax);
#ifdef verbose
            sendMessage("ACK_1",new MessageOf<ackData>(ACK_SP_MSG,v),parent,delayMin,delayDelta);
#else
            sendMessage(new MessageOf<ackData>(ACK_SP_MSG,v),parent,delayMin,delayDelta);
#endif // verbose
        }
    }
#ifdef verbose
    console << "N:";
    for (size_t i=0; i<nbreNeighborsMax; i++) {
        if (i==electedChild) console << "E ";
        else if (module->getInterface(i)==parent) console << "P ";
        else console << tabChildren[i] << " ";
    }
    console << "\n";
#endif // verbose
}

void ABCcenterCode::myGo2BFunc(const MessageOf<uint8_t>*msg,P2PNetworkInterface *sender) {
    uint8_t data = *msg->getData();
#ifdef verbose
    console << "receives Go2B d=<" << (int)data << "> from " << sender->getConnectedBlockId() << " (" << module->getDirection(sender) << ")\n";
#endif
    if (electedChild!=UINT8_MAX) {
#ifdef verbose
        char str[10];
        sprintf(str,"GO-%d",currentStep);
        sendMessage(str,new MessageOf<uint8_t>(GO2B_MSG,data),module->getInterface(electedChild),delayMin,delayDelta);
#else
        sendMessage(new MessageOf<uint8_t>(GO2B_MSG,data),module->getInterface(electedChild),delayMin,delayDelta);
#endif // verbose
    } else {
        switch (currentStep) {
            case 1 : {
            // calculate the distance to B
                initModuleData();
                moduleDistance=0; /// distance of the master
                distance2ABC[1]=0;
                currentStep=2;
                SPdata sp(0,2);
#ifdef verbose
                sendMessageToAllNeighbors("SP",new MessageOf<SPdata>(SPINNING_TREE_MSG,sp),delayMin,delayDelta,0);
#else
                sendMessageToAllNeighbors(new MessageOf<SPdata>(SPINNING_TREE_MSG,sp),delayMin,delayDelta,0);
#endif // verbose
            } break;
            case 2 : {
            // calculate the distance to C
                initModuleData();
                moduleDistance=0; /// distance of the master
                distance2ABC[2]=0;
                currentStep=3;
                SPdata sp(0,3);
#ifdef verbose
                sendMessageToAllNeighbors("SP",new MessageOf<SPdata>(SPINNING_TREE_MSG,sp),delayMin,delayDelta,0);
#else
                sendMessageToAllNeighbors(new MessageOf<SPdata>(SPINNING_TREE_MSG,sp),delayMin,delayDelta,0);
#endif // verbose
            } break;
        }
    }
    if (distance2ABC[0]==0) setColor(RED);
    else if (distance2ABC[1]==0) setColor(LIGHTGREEN);
    else if (distance2ABC[2]==0) setColor(BLUE);
    else if (abs(distance2ABC[1]-distance2ABC[2])<=1) setColor(GREY);
    else setColor(LIGHTGREY);

#ifdef verbose
    console << "-----STEP-" << (int)(currentStep) << "---<" << distance2ABC[0] << "," << distance2ABC[1] << "," << distance2ABC[2] << ">---\n";
#endif // verbose
}


void ABCcenterCode::initModuleData() {
    for (size_t i=0; i<nbreNeighborsMax; i++) {
        tabChildren[i]=(module->getInterface(i)->connectedInterface==NULL?NotConnected:StateNotKnown);
    }
    moduleDistance=UINT16_MAX; /// unknown;
    currentDistanceMax=0;
    parent = NULL;
    electedChild=UINT8_MAX; /// unknown
    currentIdMax=0;
}

void _mySpinningTreeFunc(GenericCodeBlock *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	ABCcenterCode *cb = (ABCcenterCode*)codebloc;
	MessageOf<SPdata>*msgType = (MessageOf<SPdata>*)msg.get();
	cb->mySpinningTreeFunc(msgType,sender);
}

void _myAckSPFunc(GenericCodeBlock *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	ABCcenterCode *cb = (ABCcenterCode*)codebloc;
	MessageOf<ackData>*msgType = (MessageOf<ackData>*)msg.get();
	cb->myAckSPFunc(msgType,sender);
}

void _myGo2BFunc(GenericCodeBlock *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	ABCcenterCode *cb = (ABCcenterCode*)codebloc;
	MessageOf<uint8_t>*msgType = (MessageOf<uint8_t>*)msg.get();
	cb->myGo2BFunc(msgType,sender);
}
