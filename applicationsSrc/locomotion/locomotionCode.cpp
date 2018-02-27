#include "locomotionCode.h"

const int messageDelay=50;
const int messageDelayError=5;
const int messageDelayCons=1;

void LocomotionCode::startup() {
	addMessageEventFunc(BFS_MSG,_BFSFunc);
	addMessageEventFunc(CONFIRM_EDGE_MSG,_ConfirmEdgeFunc);
	addMessageEventFunc(CUT_OFF_MSG,_CutOffFunc);
	addMessageEventFunc(AVAILABLE_MSG,_AvailableFunc);
	addMessageEventFunc(CONFIRM_PATH_MSG,_ConfirmPathFunc);
	addMessageEventFunc(CONFIRM_STREAMLINE_MSG,_ConfirmStreamlineFunc);
	
	console << "start " << module->blockId << "," << module->color << "\n";
	
	mainPathState = NONE;
	aug1PathState = NONE;
	aug2PathState = NONE;
	mainPathIn = 0;
	aug1PathIn = 0;
	aug2PathIn = 0;
	isSource=false;
	isSink=false;
			
	// if module is in Source
	if (!target->isInTarget(module->position) && module->color[1]!=1.0) {
		console << "isSource\n";
		module->setColor(RED);
		mainPathState = BFS;
		mainPathIn = module->blockId;
		mainPathOld.push_back(module->blockId);
		vector2string(mainPathOut,module->getGlBlock()->popupString);
		isSource=true;
		int t0=messageDelay;
		sendMessageToAllNeighbors("BFS",new MessageOf<bID>(BFS_MSG,module->blockId),t0,messageDelayError,0);
	} else if (module->color[1]==1.0) { // simulate virtual sink with initial green blocks
		console << "isSink\n";
		isSink=true;
		module->setColor(GREEN);
	} else {
		console << "isModule\n";
		module->setColor(GREY);
	}
}

/** Processing BFS messages **/
void LocomotionCode::ProcBFS(const MessageOf<bID>*msg, P2PNetworkInterface*sender) {
	vector<bID> pathsOld;
	bID msgData = *(msg->getData());
	bID msgFrom = sender->getConnectedBlockBId();
	console << "rec. BFS(" << msgData << ") from " << msgFrom << "\n";	
	// pathsOld = mainPathsOld U aug1PathsOld U aug2PathsOld
	pathsOld += mainPathOld;
	pathsOld += aug1PathOld;
	pathsOld += aug2PathOld;
	
/**DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG**/	
	OUTPUT << "mainPathState=" << mainPathState << endl;
	OUTPUT << "mainPathIn=" << mainPathIn << endl;
	OUTPUT << "mainPathOut={";
	vector<bID>::iterator it = mainPathOut.begin();
	while (it!=mainPathOut.end()) {
		OUTPUT << (*it) << ",";
		it++;
	}
	OUTPUT << "}"<< endl;
	OUTPUT << "pathsOld={";
	it = pathsOld.begin();
	while (it!=pathsOld.end()) {
		OUTPUT << (*it) << ",";
		it++;
	}
	OUTPUT << "}"<< endl;
/**DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG**/	
	
	if (mainPathState==NONE && !isIn(pathsOld,msgData)) {
		mainPathOld.push_back(msgData);
		int t0=messageDelay;
		OUTPUT << t0 << endl;
		sendMessage("ConfirmEdge",new Message(CONFIRM_EDGE_MSG),sender,t0,messageDelayError);
		if (isSink) {
			mainPathState = ConfPath;
			mainPathIn = msgFrom;
			mainPathOut.clear();
			mainPathOut.push_back(module->blockId);
			module->setColor(GREEN);
			sendMessage("ConfirmPath",new Message(CONFIRM_PATH_MSG),sender,t0,messageDelayError);
		} else {
			mainPathState = BFS;
			mainPathIn = msgFrom;
			mainPathOut.clear();
			module->setColor(isSource?RED:ORANGE);
			t0+=messageDelayCons;
			sendMessageToAllNeighbors("BFS",new MessageOf<bID>(BFS_MSG,msgData),t0,messageDelayError,0);
		}
		vector2string(mainPathOut,module->getGlBlock()->popupString);
	} else if (mainPathState==Streamline && aug1PathState==NONE && msgFrom!=mainPathIn && msgFrom!=mainPathOut && !isIn(pathsOld,msgData)) {
/**DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG**/	
	OUTPUT << "aug1PathState=" << aug1PathState << endl;
	OUTPUT << "aug1PathIn=" << aug1PathIn << endl;
	OUTPUT << "aug1PathOut={";
	vector<bID>::iterator it = aug1PathOut.begin();
	while (it!=aug1PathOut.end()) {
		OUTPUT << (*it) << ",";
		it++;
	}
	OUTPUT << "}"<< endl;
/**DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG**/	
		aug1PathOld.push_back(msgData);
		int t0=messageDelay;
		sendMessage("ConfirmEdge",new Message(CONFIRM_EDGE_MSG),sender,t0,messageDelayError);
		aug1PathState = BFS;
		aug1PathIn = msgFrom;
		aug1PathOut.clear();
		t0+=messageDelayCons;
		P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(mainPathIn);
		if (p2p!=NULL) {
			sendMessage("BFS",new MessageOf<bID>(BFS_MSG,msgData),p2p,t0,messageDelayError);
		} else {
			OUTPUT << "WARNING: " << mainPathIn << " is not neighbor of " << module->blockId << endl;
		}
	} else if (mainPathState==Streamline && aug2PathState==NONE && msgFrom==mainPathOut) {
		aug2PathOld.push_back(msgData);
		int t0=messageDelay;
		sendMessage("ConfirmEdge",new Message(CONFIRM_EDGE_MSG),sender,t0,messageDelayError);
		aug2PathState = BFS;
		aug2PathIn = msgFrom;
		aug2PathOut.clear();
		t0+=messageDelayCons;
		sendMessageToAllNeighbors("BFS",new MessageOf<bID>(BFS_MSG,msgData),t0,messageDelayError,1,sender);
	}
};

/** Processing ConfirmEdge messages **/
void LocomotionCode::ProcConfirmEdge(P2PNetworkInterface* sender) {
	bID msgFrom = sender->getConnectedBlockBId();

	console << "rec. ConfirmEdge\n";
	if (mainPathState==BFS) {
		mainPathOut.push_back(msgFrom);
		vector2string(mainPathOut,module->getGlBlock()->popupString);
		module->setColor(isSource?RED:CYAN);
	} else if (aug1PathState==BFS) {
		aug1PathOut.push_back(msgFrom);
	} else if (aug2PathState==BFS) {
		aug2PathOut.push_back(msgFrom);
	} else {
		int t0=messageDelay;
		sendMessage("CutOff",new Message(CUT_OFF_MSG),sender,t0,messageDelayError);
	}
}

/** Processing CutOff messages **/
void LocomotionCode::ProcCutOff(P2PNetworkInterface* sender) {
	bID msgFrom = sender->getConnectedBlockBId();
	console << "rec. CutOff from " << msgFrom << "\n";

/**DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG**/	
	OUTPUT << "mainPathIn=" << mainPathIn << endl;
	OUTPUT << "mainPathOut={";
	vector<bID>::iterator it = mainPathOut.begin();
	while (it!=mainPathOut.end()) {
		OUTPUT << (*it) << ",";
		it++;
	}
	OUTPUT << "}"<< endl;
	OUTPUT << "mainPathOld={";
	it = mainPathOld.begin();
	while (it!=mainPathOld.end()) {
		OUTPUT << (*it) << ",";
		it++;
	}
	OUTPUT << "}"<< endl;
/**DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG**/	

// treatements
	bool isMainPathRemoved=(mainPathState!=NONE && msgFrom==mainPathIn);
	OUTPUT << "isMainPathRemoved=" << isMainPathRemoved << endl;
	int t0=messageDelay;
	if (isMainPathRemoved) {
		sendMessageToPath("CutOff",CUT_OFF_MSG,mainPathOut,0);
		t0+=messageDelayCons;
		mainPathState=NONE;
		mainPathIn=0;
		mainPathOut.clear();
		module->getGlBlock()->popupString="";
		module->setColor(ORANGE);
	}
	if (aug1PathState!=NONE && (isMainPathRemoved || msgFrom==aug1PathIn)) {
		sendMessageToPath("CutOff",CUT_OFF_MSG,aug1PathOut,0);
		t0+=messageDelayCons;
		aug1PathState=NONE;
		aug1PathIn=0;
		aug1PathOut.clear();
	}
	if (aug2PathState!=NONE && (isMainPathRemoved || msgFrom==aug2PathIn)) {
		sendMessageToPath("CutOff",CUT_OFF_MSG,aug2PathOut,0);
		t0+=messageDelayCons;
		aug2PathState=NONE;
		aug2PathIn=0;
		aug2PathOut.clear();
	}
	sendMessageToAllNeighbors("Available",new Message(AVAILABLE_MSG),t0,messageDelayError,0);	
}

/** Processing Available messages **/
void LocomotionCode::ProcAvailable(P2PNetworkInterface* sender) {
	bID msgFrom = sender->getConnectedBlockBId();
	console << "rec. Available from " << msgFrom << "\n";

/**DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG**/	
	OUTPUT << "mainPathState=" << mainPathState << endl;
	OUTPUT << "mainPathIn=" << mainPathIn << endl;
	OUTPUT << "mainPathOut={";
	vector<bID>::iterator it = mainPathOut.begin();
	while (it!=mainPathOut.end()) {
		OUTPUT << (*it) << ",";
		it++;
	}
	OUTPUT << "}"<< endl;
	OUTPUT << "mainPathOld={";
	it = mainPathOld.begin();
	while (it!=mainPathOld.end()) {
		OUTPUT << (*it) << ",";
		it++;
	}
	OUTPUT << "}"<< endl;
/**DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG**/	

	int t0=messageDelay;
	if (mainPathState==BFS) {
		if (!mainPathOld.empty()) {
			sendMessage("BFS",new MessageOf<bID>(BFS_MSG,mainPathOld.front()),sender,t0,messageDelayError);
		}
	} else if (aug1PathState==BFS && msgFrom==mainPathIn) {
		if (!aug1PathOld.empty()) {
			sendMessage("BFS",new MessageOf<bID>(BFS_MSG,aug1PathOld.front()),sender,t0,messageDelayError);
		}
	} else if (aug2PathState==BFS && msgFrom==mainPathOut) {
		if (!aug2PathOld.empty()) {
			sendMessage("BFS",new MessageOf<bID>(BFS_MSG,aug2PathOld.front()),sender,t0,messageDelayError);
		}
	}
}

/** Processing the "ConfirmPath" message type **/
void LocomotionCode::ProcConfirmPath(P2PNetworkInterface* sender) {
	bID msgFrom = sender->getConnectedBlockBId();
	console << "rec. ConfirmPath from " << msgFrom << "\n";
	
/**DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG**/	
	OUTPUT << "mainPathState=" << mainPathState << endl;
	OUTPUT << "mainPathIn=" << mainPathIn << endl;
	OUTPUT << "mainPathOut={";
	vector<bID>::iterator it = mainPathOut.begin();
	while (it!=mainPathOut.end()) {
		OUTPUT << (*it) << ",";
		it++;
	}
	OUTPUT << "}"<< endl;
/**DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG****DEBUG**/	

	if (mainPathState==BFS && isIn(mainPathOut,msgFrom)) {
		int t0=messageDelay;
		sendMessageToPath("CutOff",CUT_OFF_MSG,mainPathOut,msgFrom);
		mainPathOut.clear();
		mainPathOut.push_back(msgFrom);
		t0+=messageDelayCons;
		if (isSource) {
			mainPathState = Streamline;
			sendMessage("ConfirmStreamline",new Message(CONFIRM_STREAMLINE_MSG),sender,t0,messageDelayError);
		} else {
			mainPathState = ConfPath;
			sendMessage("ConfirmPath",new Message(CONFIRM_PATH_MSG),module->getP2PNetworkInterfaceByDestBlockId(mainPathIn),t0,messageDelayError);
		}
	} else if (aug1PathState==BFS && msgFrom==mainPathIn) {
		aug1PathOut.clear();
		aug1PathOut.push_back(msgFrom);
		aug1PathState=ConfPath;
		int t0=messageDelay;
		sendMessage("ConfirmPath",new Message(CONFIRM_PATH_MSG),module->getP2PNetworkInterfaceByDestBlockId(aug1PathIn),t0,messageDelayError);
	} else if (aug2PathState==BFS && (aug1PathState!=ConfPath || msgFrom!=mainPathIn)) {
		sendMessageToPath("CutOff",CUT_OFF_MSG,aug2PathOut,msgFrom);
		aug2PathOut.clear();
		aug2PathOut.push_back(msgFrom);
		aug2PathState=ConfPath;
		int t0=messageDelay;
		sendMessage("ConfirmPath",new Message(CONFIRM_PATH_MSG),module->getP2PNetworkInterfaceByDestBlockId(aug2PathIn),t0,messageDelayError);
	}
}

/** Processing the "ConfirmStreamline" message type **/
void LocomotionCode::ProcConfirmStreamline(P2PNetworkInterface* sender) {
	bID msgFrom = sender->getConnectedBlockBId();
	console << "rec. ConfirmStreamline from " << msgFrom << "\n";
	if (mainPathState==ConfPath && msgFrom==mainPathIn) {
		mainPathState=Streamline;
		int t0=messageDelay;
		if (!isSink) {
			sendMessage("ConfirmStreamline",new Message(CONFIRM_STREAMLINE_MSG),module->getP2PNetworkInterfaceByDestBlockId(aug2PathIn),t0,messageDelayError);
			t0+=messageDelayCons;
		}
		// send to neighbors but mainPathIn && mainPathOut
		P2PNetworkInterface *p2pMainPathIn= module->getP2PNetworkInterfaceByDestBlockId(mainPathIn);
		P2PNetworkInterface *p2p;
		for (int i=0; i<hostBlock->getNbInterfaces(); i++) {
			p2p = hostBlock->getInterface(i);	
			if(p2p->connectedInterface) { // on regarde si elle n'est pas dans les interdits
				if (p2p!=p2pMainPathIn && !isIn(mainPathOut,p2p->getConnectedBlockBId())) {
					sendMessage("Available",new Message(AVAILABLE_MSG), p2p, t0, messageDelayError);
					t0+=messageDelayCons;
				}
			}
		}
	} else if (aug1PathState==ConfPath && msgFrom==aug1PathIn) {
		mainPathIn = aug1PathIn;
		sendMessageToPath("ConfirmStreamline",CONFIRM_STREAMLINE_MSG,aug1PathOut,0);

		// SendAround(aug1PathOut ∪ aug1PathIn, {”Available”, ∅})
		// send to neighbors but aug1PathIn && aug1PathOut
		P2PNetworkInterface *p2pAug1PathIn= module->getP2PNetworkInterfaceByDestBlockId(aug1PathIn);
		P2PNetworkInterface *p2p;
		int t0=messageDelay;
		for (int i=0; i<hostBlock->getNbInterfaces(); i++) {
			p2p = hostBlock->getInterface(i);
			if(p2p->connectedInterface) { // on regarde si elle n'est pas dans les interdits
				if (p2p!=p2pAug1PathIn && !isIn(aug1PathOut,p2p->getConnectedBlockBId())) {
					sendMessage("Available",new Message(AVAILABLE_MSG), p2p, t0, messageDelayError);
					t0+=messageDelayCons;
				}
			}
		}
		aug1PathState = NONE;
		aug1PathIn = 0;
		aug1PathOut.clear();
	} else if (aug2PathState==ConfPath && msgFrom==aug2PathIn) {
		sendMessageToPath("ConfirmStreamline",CONFIRM_STREAMLINE_MSG,aug2PathOut,0);

		// SendAround(aug2PathOut ∪ aug2PathIn, {”Available”, ∅})
		// send to neighbors but aug2PathIn && aug2PathOut
		P2PNetworkInterface *p2pAug2PathIn= module->getP2PNetworkInterfaceByDestBlockId(aug2PathIn);
		P2PNetworkInterface *p2p;
		int t0=messageDelay;
		for (int i=0; i<hostBlock->getNbInterfaces(); i++) {
			p2p = hostBlock->getInterface(i);
			if(p2p->connectedInterface) { // on regarde si elle n'est pas dans les interdits
				if (p2p!=p2pAug2PathIn && !isIn(aug2PathOut,p2p->getConnectedBlockBId())) {
					sendMessage("Available",new Message(AVAILABLE_MSG), p2p, t0, messageDelayError);
					t0+=messageDelayCons;
				}
			}
		}
		if (mainPathIn==aug2PathOut) {
			mainPathState = NONE;
			mainPathIn = 0; 
			mainPathOut.clear();
			module->getGlBlock()->popupString="";
			module->setColor(ORANGE);
			aug1PathState = NONE; 
			aug1PathIn = 0; 
			aug1PathOut.clear();
		} else {
			mainPathOut=aug2PathOut;
			vector2string(mainPathOut,module->getGlBlock()->popupString);
			module->setColor(CYAN);
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
	MessageOf<bID>*msgType = (MessageOf<bID>*)msg.get();
	cb->ProcBFS(msgType,sender);
}

void _ConfirmEdgeFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	LocomotionCode *cb = (LocomotionCode*)codebloc;
	cb->ProcConfirmEdge(sender);
}

void _CutOffFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	LocomotionCode *cb = (LocomotionCode*)codebloc;
	cb->ProcCutOff(sender);
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

void LocomotionCode::sendMessageToPath(const string& str, int msgType, vector<bID>& path,bID exception) {
	OUTPUT << "sendMessageToPath({";
	vector<bID>::iterator it = path.begin();
	while (it!=path.end()) {
		OUTPUT << (*it) << ",";
		it++;
	}
	OUTPUT << "} but" << exception << ")"<< endl;
	
	if (!path.empty()) {
		it=path.begin();
		int t0=messageDelay;
		while (it!=path.end()) {
			if (*it!=exception) {
				P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(*it);
				OUTPUT << "p2p=" << ((p2p==NULL)?0:p2p->getConnectedBlockBId()) << endl;
				if (p2p) sendMessage(str.c_str(),new Message(msgType),p2p,t0,messageDelayError);
				t0+=messageDelayCons;
			}
			it++;
		}
	}
}

void vector2string(const std::vector<bID>&v,string &s) {
	vector<bID>::const_iterator it = v.begin();
	s.clear();
	while (it!=v.end()) {
		s+= to_string(*it) + ",";
		it++;
	}
}