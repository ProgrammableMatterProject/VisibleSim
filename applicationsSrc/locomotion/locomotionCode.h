#ifndef locomotionCode_H_
#define locomotionCode_H_
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"

static const int BFS_MSG=1001;
static const int CONFIRM_EDGE_MSG=1002;
static const int CUT_OFF_MSG=1003;
static const int AVAILABLE_MSG=1004;
static const int CONFIRM_PATH_MSG=1005;
static const int CONFIRM_STREAMLINE_MSG=1005;

using namespace RobotBlocks;

enum PathState {NONE, BFS, ConfPath, ConfStreamline, Streamline};  

class PathMessageData {
public :
	bool firstExcepted;
	vector<int> path;
	
	PathMessageData() { firstExcepted=false; };
	bool empty() { 
		return path.empty();
	}
};

class LocomotionCode : public RobotBlocksBlockCode {
private:
	RobotBlocksBlock *module;
	PathState mainPathState;	//! state of the main path: {NONE, BFS, ConfPath, Streamline}
	int mainPathIn;				//! ID of parent meta-module on the main tree
	vector<int>mainPathOut; 	//! ID’s of child meta-modules on the main tree
	PathState aug1PathState;	//! state of the augmenting path 1: {NONE, BFS, ConfPath}
	int aug1PathIn;				//! ID of parent meta-module on the augmenting tree (type 1)
	vector<int>aug1PathOut;		//! ID’s of child meta-modules on the augmenting tree (type 1)
	PathState aug2PathState;	//! state of the augmenting path 2: {NONE, BFS, ConfPath}
	int aug2PathIn;				//! ID of parent meta-module on the augmenting tree (type 2)
	vector<int>aug2PathOut;		//! ID’s of child meta-modules on the augmenting tree (type 2)
	vector<int>pathsOld;		//! list of path ID’s that has been processed in current step
	bool isSource;				//! Is meta-module a source.
	bool isSink;

public :
	LocomotionCode(RobotBlocksBlock *host):RobotBlocksBlockCode(host) { module=host; };
	~LocomotionCode() {};

	void startup();
	void ProcBFS(const MessageOf<int>*msg,P2PNetworkInterface *sender);
	void ProcConfirmEdge(P2PNetworkInterface *sender);
	void ProcCutOff(const MessageOf<PathMessageData>*msg,P2PNetworkInterface *sender);
	void ProcAvailable(P2PNetworkInterface *sender);
	void ProcConfirmPath(P2PNetworkInterface *sender);
	void ProcConfirmStreamline(P2PNetworkInterface *sender);

	void sendMessageToPath(const string &str, int msgType,vector<int> &path,int time,int delta,bool firstExcepted=false);
	unsigned int lastId(const vector<int> &path);
/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
	    return(new LocomotionCode((RobotBlocksBlock*)host));
	};
/*****************************************************************************/
};

void _BFSFunc(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _ConfirmEdgeFunc(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _CutOffFunc(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _AvailableFunc(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _ConfirmPathFunc(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _ConfirmStreamlineFunc(BlockCode*,MessagePtr,P2PNetworkInterface *sender);

template <typename T> void operator+=(std::vector<T> &v1, const std::vector<T> &v2);
template <typename T> bool isIn(const std::vector<T> &v,T value);
template <typename T> bool operator==(T value,const std::vector<T> &v);
template <typename T> bool operator!=(T value,const std::vector<T> &v);
#endif /* locomotionCode_H_ */
