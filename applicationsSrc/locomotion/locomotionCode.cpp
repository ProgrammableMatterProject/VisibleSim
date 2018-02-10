#include "locomotionCode.h"

const int messageDelay=1000;
const int messageDelayError=0;

void LocomotionCode::startup() {
	addMessageEventFunc(BFS_MSG,_BFSFunc);
	addMessageEventFunc(CONFIRM_EDGE_MSG,_ConfirmEdgeFunc);
	addMessageEventFunc(CUT_OFF_MSG,_CutOffFunc);
	addMessageEventFunc(AVAILABLE_MSG,_AvailableFunc);
	addMessageEventFunc(CONFIRM_PATH_MSG,_ConfirmPathFunc);
	addMessageEventFunc(CONFIRM_STREAMLINE_MSG,_ConfirmStreamlineFunc);
	
	console << "start " << module->blockId << "\n";
	
	mainPathState = NONE;
	mainPathIn = module->blockId;
	mainPathOut.push_back(module->blockId);
		
	// if module is in Source
	if (!target->isInTarget(module->position)) {
		module->setColor(RED);
		
		mainPathState = BFS;
		isSource=true;
		int t0=scheduler->now()+messageDelay;
		sendMessageToAllNeighbors("BFS",new MessageOf<int>(BFS_MSG,module->blockId),t0,messageDelayError,0);
	} else {
		module->setColor(GREY);
	}
}

/** Processing BFS messages **/
void LocomotionCode::ProcBFS(const MessageOf<int>*msg, P2PNetworkInterface*sender) {
	console << "rec. BFS\n";
	
	vector<int> pathsOld;
	
	// pathsOld = mainPathsOld U aug1PathsOld U aug2PathsOld
	pathsOld = mainPathOut;
	pathsOld += aug1PathOut;
	pathsOld += aug2PathOut;
	int msgData = *msg->getData();
	int msgFrom = sender->getConnectedBlockId();
	if (mainPathState==NONE && !isIn(pathsOld,msgData)) {
		module->setColor(Colors[msgData % NB_COLORS]);
		mainPathOut.push_back(msgData);
		int t0=scheduler->now()+messageDelay;
		sendMessage("ConfirmEdge",new Message(CONFIRM_EDGE_MSG),sender,t0,messageDelayError);
		if (isSink) {
			mainPathState = ConfPath;
			mainPathIn = sender->getConnectedBlockId();
			mainPathOut.clear();
			mainPathOut.push_back(module->blockId);
			int t0=scheduler->now()+messageDelay;
			sendMessage("ConfirmPath",new Message(CONFIRM_PATH_MSG),sender,t0,messageDelayError);
		} else {
			mainPathState = BFS;
			mainPathIn = sender->getConnectedBlockId();
			mainPathOut.clear();
			int t0=scheduler->now()+messageDelay;
			sendMessageToAllNeighbors("FBS",new MessageOf<int>(BFS_MSG,msgData),t0,messageDelayError,0);
		}
	} else if (mainPathState==Streamline && aug1PathState==NONE && msgFrom!=mainPathIn && msgFrom!=mainPathOut && !isIn(pathsOld,msgData)) {
		aug1PathOut.push_back(msgData);
		int t0=scheduler->now()+messageDelay;
		sendMessage("ConfirmEdge",new Message(CONFIRM_EDGE_MSG),sender,t0,messageDelayError);
		aug1PathState = BFS;
		aug1PathIn = msgFrom;
		aug1PathOut.clear();
		t0=scheduler->now()+messageDelay;
		sendMessage("FBS",new MessageOf<int>(BFS_MSG,msgData),module->getP2PNetworkInterfaceByDestBlockId(mainPathIn),t0,messageDelayError);
	} else if (mainPathState==Streamline && aug2PathState==NONE && msgFrom==mainPathOut) {
		aug2PathOut.push_back(msgData);
		int t0=scheduler->now()+messageDelay;
		sendMessage("ConfirmEdge",new Message(CONFIRM_EDGE_MSG),sender,t0,messageDelayError);
		aug2PathState = BFS;
		aug2PathIn = msgFrom;
		aug2PathOut.clear();
		t0=scheduler->now()+messageDelay;
		sendMessageToAllNeighbors("FBS",new MessageOf<int>(BFS_MSG,msgData),t0,messageDelayError,1,sender);
	}
};

/** Processing ConfirmEdge messages **/
void LocomotionCode::ProcConfirmEdge(P2PNetworkInterface* sender) {
	console << "rec. ConfirmEdge\n";
	if (mainPathState==ConfPath) {
		mainPathOut.push_back(sender->getConnectedBlockId());
	} else if (aug1PathState==ConfPath) {
		aug1PathOut.push_back(sender->getConnectedBlockId());
	} else if (aug2PathState==ConfPath) {
		aug2PathOut.push_back(sender->getConnectedBlockId());
	} else {
		int t0=scheduler->now()+messageDelay;
		sendMessage("CutOff",new Message(CUT_OFF_MSG),sender,t0,messageDelayError);
	}
}

/** Processing ConfirmEdge messages **/
void LocomotionCode::ProcCutOff(const MessageOf<PathMessageData> *msg,P2PNetworkInterface* sender) {
	console << "rec. CutOff\n";
	PathMessageData *msgData = msg->getData();
	if (!msgData->empty()) {
		int t0=scheduler->now()+messageDelay;
		sendMessageToPath("CutOff",CUT_OFF_MSG,msgData->path,t0,messageDelayError);
		if (msgData->firstExcepted) return; // in this case, no treatment for the message, just re-send it !
	}
	bool isMainPathRemoved=false;
	if (mainPathState!=NONE and sender->getConnectedBlockId()==mainPathIn) {
		int t0=scheduler->now()+messageDelay;
		sendMessageToPath("CutOff",CUT_OFF_MSG,mainPathOut,t0,messageDelayError);
		mainPathState=NONE;
		mainPathIn=0;
		mainPathOut.clear();
		isMainPathRemoved=true;
	}
	if (aug1PathState!=NONE && (isMainPathRemoved || sender->getConnectedBlockId()==aug1PathIn)) {
		int t0=scheduler->now()+messageDelay;
		sendMessageToPath("CutOff",CUT_OFF_MSG,aug1PathOut,t0,messageDelayError);
		aug1PathState=NONE;
		aug1PathIn=0;
		aug1PathOut.clear();
	}
	if (aug2PathState!=NONE && (isMainPathRemoved || sender->getConnectedBlockId()==aug2PathIn)) {
		int t0=scheduler->now()+messageDelay;
		sendMessageToPath("CutOff",CUT_OFF_MSG,aug2PathOut,t0,messageDelayError);
		aug2PathState=NONE;
		aug2PathIn=0;
		aug2PathOut.clear();
	}
	int t0=scheduler->now()+messageDelay;
	sendMessageToAllNeighbors("Available",new Message(AVAILABLE_MSG),t0,messageDelayError,0);	
}

/** Processing Available messages **/
void LocomotionCode::ProcAvailable(P2PNetworkInterface* sender) {
	console << "rec. Available\n";
	if (mainPathState==ConfPath) {
		int t0=scheduler->now()+messageDelay;
		sendMessage("BFS",new MessageOf<int>(BFS_MSG,mainPathOut.front()),sender,t0,messageDelayError);
	} else if (aug1PathState==ConfPath && sender->getConnectedBlockId()==mainPathIn) {
		int t0=scheduler->now()+messageDelay;
		sendMessage("BFS",new MessageOf<int>(BFS_MSG,aug1PathOut.front()),sender,t0,messageDelayError);		
	} else if (aug2PathState==ConfPath && sender->getConnectedBlockId()==mainPathOut) {
		int t0=scheduler->now()+messageDelay;
		sendMessage("BFS",new MessageOf<int>(BFS_MSG,aug2PathOut.front()),sender,t0,messageDelayError);		
	}
}

/** Processing the "ConfirmPath" message type **/
void LocomotionCode::ProcConfirmPath(P2PNetworkInterface* sender) {
	int msgFrom = sender->getConnectedBlockId();
	if (mainPathState==ConfPath && isIn(mainPathOut,msgFrom)) {
		int t0=scheduler->now()+messageDelay;
		sendMessageToPath("CutOff",CUT_OFF_MSG,mainPathOut,t0,messageDelayError,true);
		if (isSource) {
			mainPathState = Streamline;
			t0=scheduler->now()+messageDelay;
			sendMessage("ConfirmStreamline",new Message(CONFIRM_STREAMLINE_MSG),sender,t0,messageDelayError);
		} else {
			mainPathState = ConfStreamline;
			t0=scheduler->now()+messageDelay;
			sendMessage("ConfirmPath",new Message(CONFIRM_PATH_MSG),module->getP2PNetworkInterfaceByDestBlockId(mainPathIn),t0,messageDelayError);
		}
	} else if (aug1PathState==ConfPath && msgFrom==mainPathIn) {
		aug1PathOut.clear();
		aug1PathOut.push_back(msgFrom);
		aug1PathState=ConfStreamline;
		int t0=scheduler->now()+messageDelay;
		sendMessage("ConfirmPath",new Message(CONFIRM_PATH_MSG),module->getP2PNetworkInterfaceByDestBlockId(aug1PathIn),t0,messageDelayError);
	} else if (aug2PathState==ConfPath && (aug1PathState!=ConfStreamline || msgFrom!=mainPathIn)) {
		int t0=scheduler->now()+messageDelay;
		sendMessageToPath("CutOff",CUT_OFF_MSG,aug2PathOut,t0,messageDelayError,true);
		aug2PathOut.clear();
		aug2PathOut.push_back(msgFrom);
		aug2PathState=ConfStreamline;
		t0=scheduler->now()+messageDelay;
		sendMessage("ConfirmPath",new Message(CONFIRM_PATH_MSG),module->getP2PNetworkInterfaceByDestBlockId(aug2PathIn),t0,messageDelayError);
	}
}

/** Processing the "ConfirmStreamline" message type **/
void LocomotionCode::ProcConfirmStreamline(P2PNetworkInterface* sender) {
	int msgFrom = sender->getConnectedBlockId();
	if (mainPathState==ConfStreamline && msgFrom==mainPathIn) {
		mainPathState=Streamline;
		if (!isSink) {
			int t0=scheduler->now()+messageDelay;
			sendMessage("ConfirmStreamline",new Message(CONFIRM_STREAMLINE_MSG),module->getP2PNetworkInterfaceByDestBlockId(aug2PathIn),t0,messageDelayError);
		}
		// send to neighbors but mainPathIn && mainPathOut
		P2PNetworkInterface *p2pMainPathIn= module->getP2PNetworkInterfaceByDestBlockId(mainPathIn);
		P2PNetworkInterface *p2p;
		int t0=scheduler->now()+messageDelay;
		for (int i=0; i<hostBlock->getNbInterfaces(); i++) {
			p2p = hostBlock->getInterface(i);
			if(p2p->connectedInterface) { // on regarde si elle n'est pas dans les interdits
				if (p2p!=p2pMainPathIn && !isIn(mainPathOut,p2p->getConnectedBlockId())) {
					sendMessage("Available",new Message(AVAILABLE_MSG), p2p, t0, messageDelayError);
				}
			}
		}
	} else if (aug1PathState==ConfStreamline && msgFrom==aug1PathIn) {
		mainPathIn = aug1PathIn;
		int t0=scheduler->now()+messageDelay;
		sendMessageToPath("ConfirmStreamline",CONFIRM_STREAMLINE_MSG,aug1PathOut,t0,messageDelayError);

		// SendAround(aug1PathOut ∪ aug1PathIn, {”Available”, ∅})
		// send to neighbors but aug1PathIn && aug1PathOut
		P2PNetworkInterface *p2pAug1PathIn= module->getP2PNetworkInterfaceByDestBlockId(aug1PathIn);
		P2PNetworkInterface *p2p;
		t0=scheduler->now()+messageDelay;
		for (int i=0; i<hostBlock->getNbInterfaces(); i++) {
			p2p = hostBlock->getInterface(i);
			if(p2p->connectedInterface) { // on regarde si elle n'est pas dans les interdits
				if (p2p!=p2pAug1PathIn && !isIn(aug1PathOut,p2p->getConnectedBlockId())) {
					sendMessage("Available",new Message(AVAILABLE_MSG), p2p, t0, messageDelayError);
				}
			}
		}
		aug1PathState = NONE;
		aug1PathIn = 0;
		aug1PathOut.clear();
	} else if (aug2PathState==ConfStreamline && msgFrom==aug2PathIn) {
		int t0=scheduler->now()+messageDelay;
		sendMessageToPath("ConfirmStreamline",CONFIRM_STREAMLINE_MSG,aug2PathOut,t0,messageDelayError);

		// SendAround(aug2PathOut ∪ aug2PathIn, {”Available”, ∅})
		// send to neighbors but aug2PathIn && aug2PathOut
		P2PNetworkInterface *p2pAug2PathIn= module->getP2PNetworkInterfaceByDestBlockId(aug2PathIn);
		P2PNetworkInterface *p2p;
		int t0=scheduler->now()+messageDelay;
		for (int i=0; i<hostBlock->getNbInterfaces(); i++) {
			p2p = hostBlock->getInterface(i);
			if(p2p->connectedInterface) { // on regarde si elle n'est pas dans les interdits
				if (p2p!=p2pAug2PathIn && !isIn(aug2PathOut,p2p->getConnectedBlockId())) {
					sendMessage("Available",new Message(AVAILABLE_MSG), p2p, t0, messageDelayError);
				}
			}
		}
		if (aug2PathOut==mainPathIn) {
			mainPathState = NONE;
			mainPathIn = 0; 
			mainPathOut.clear();
			aug1PathState = NONE; 
			aug1PathIn = 0; 
			aug1PathOut.clear();
		} else
			mainPathOut=aug2PathOut;
		}
		aug2PathState = NONE; 
		aug2PathIn = 0; 
		aug2PathOut.clear();
	}
}

/*************************************************************************************************************/
/*************************************************************************************************************/
void _BFSFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	LocomotionCode *cb = (LocomotionCode*)codebloc;
	MessageOf<int>*msgType = (MessageOf<int>*)msg.get();
	cb->ProcBFS(msgType,sender);
}

void _ConfirmEdgeFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	LocomotionCode *cb = (LocomotionCode*)codebloc;
	cb->ProcConfirmEdge(sender);
}

void _CutOffFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	LocomotionCode *cb = (LocomotionCode*)codebloc;
	MessageOf<PathMessageData>*msgType = (MessageOf<PathMessageData>*)msg.get();
	cb->ProcCutOff(msgType,sender);
}

void _AvailableFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	LocomotionCode *cb = (LocomotionCode*)codebloc;
	cb->ProcAvailable(sender);
}

void _ConfirmPathFunc(BlockCode*codebloc, MessagePtr msg, P2PNetworkInterface* sender) {
	LocomotionCode *cb = (LocomotionCode*)codebloc;
	cb->ProcConfirmPath(sender);	
}

void _ConfirmStreamlineFunc(BlockCode*codebloc, MessagePtr msg, P2PNetworkInterface* sender) {
	LocomotionCode *cb = (LocomotionCode*)codebloc;
	cb->ProcConfirmStreamline(sender);	
}

template <typename T>
void operator+=(std::vector<T> &v1, const std::vector<T> &v2) {
    v1.insert(v1.end(), v2.begin(), v2.end());
}

template <typename T>
bool operator==(T value,const std::vector<T> &v) {
    return v.size()==1 && v.front()==value; 
}

template <typename T>
bool operator!=(T value,const std::vector<T> &v) {
    return v.size()!=1 || v.front()!=value; 
}


template <typename T>
bool isIn(const std::vector<T> &v,T value) {
	typename vector<T>::const_iterator current=v.begin();
	while (current!=v.end()) {
		if (*current==value) return true;
		++current;
	}
	return false;
}

void LocomotionCode::sendMessageToPath(const string& str, int msgType, vector<int>& path, int time, int delta, bool firstExcepted) {
	if (!path.empty()) { 
		int lenght = path.size();
		PathMessageData *msg = new PathMessageData();
		msg->firstExcepted = firstExcepted;
		vector<int>::iterator it=path.begin();
		for (int i=0; i<lenght-1; i++,it++) {
			msg->path.push_back(*it);
		}
		P2PNetworkInterface *next = module->getP2PNetworkInterfaceByDestBlockId(*it);
		sendMessage(str.c_str(),new MessageOf<PathMessageData>(msgType,*msg),next,time,delta);
	}
}
