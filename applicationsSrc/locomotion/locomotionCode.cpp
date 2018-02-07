#include "locomotionCode.h"

template <typename T>
void operator+=(std::vector<T> &v1, const std::vector<T> &v2) {
    v1.insert(v1.end(), v2.begin(), v2.end());
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

void LocomotionCode::startup() {
	addMessageEventFunc(BFS_MSG,_BFSFunc);
	console << "start " << module->blockId << "\n";
	
	// if module is in Source
	if (target->isInTarget(module->position)) {
		module->setColor(GREY);
		
		mainPathState = BFS;
		mainPathIn = module->blockId;
		mainPathsOld.push_back(module->blockId);
		
		int t0=scheduler->now()+1000;
		sendMessageToAllNeighbors("BFS",new MessageOf<int>(BFS_MSG,module->blockId),t0,0,0);
	} else {
		module->setColor(RED);
	}
}

/** Processing BFS messages **/
void LocomotionCode::ProcBFS(const MessageOf<int>*msg, P2PNetworkInterface*sender) {
	console << "rec. BFS\n";
	
	vector<int> pathsOld;
	
	// pathsOld = mainPathsOld U
	pathsOld = mainPathsOld;
	pathsOld += aug1PathsOld;
	pathsOld += aug2PathsOld;
	
	
	if (mainPathState==NONE && !isIn(pathsOld,*msg->getData())) {
		
	}
};

void _BFSFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	LocomotionCode *cb = (LocomotionCode*)codebloc;
	MessageOf<int>*msgType = (MessageOf<int>*)msg.get();
	cb->ProcBFS(msgType,sender);
}


