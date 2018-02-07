#ifndef locomotionCode_H_
#define locomotionCode_H_
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"

static const int BFS_MSG=1001;

using namespace RobotBlocks;

enum PathState {NONE, BFS, ConfPath, StreamLine};  

class MessageData {
public :
	uint16_t mainPathIn;
};

class LocomotionCode : public RobotBlocksBlockCode {
private:
	RobotBlocksBlock *module;
	PathState mainPathState;
	int mainPathIn;
	vector<int>mainPathOut;
	vector<int>mainPathsOld;
	vector<int>aug1PathsOld;
	vector<int>aug2PathsOld;
	
public :
	LocomotionCode(RobotBlocksBlock *host):RobotBlocksBlockCode(host) { module=host; };
	~LocomotionCode() {};

	void startup();
	void ProcBFS(const MessageOf<int>*msg,P2PNetworkInterface *sender);

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
	    return(new LocomotionCode((RobotBlocksBlock*)host));
	};
/*****************************************************************************/
};

void _BFSFunc(BlockCode*,MessagePtr,P2PNetworkInterface *sender);

#endif /* locomotionCode_H_ */
