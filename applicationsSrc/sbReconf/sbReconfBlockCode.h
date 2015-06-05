/*
 * SbReconfBlockCode.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef SBRECONFBLOCKCODE_H_
#define SBRECONFBLOCKCODE_H_

#define MAP_MSG_ID			9001
#define ACKMAP_MSG_ID		9002
#define HEAD_MSG_ID			9003
#define HBCK_MSG_ID		    9103
#define END_MSG_ID			9004
#define TRAIN_READY_MSG_ID	9005
#define CREATE_LINE_MSG_ID	9006
#define SET_RDV_MSG_ID		9007
#define UNLOCK_MSG_ID		9008
#define RECONNECT_MSG_ID	9009
/*#define DISABLE_MSG_ID		9010
#define NOACTIVITY_MSG_ID	9011
#define REINIT_MSG_ID		9012
#define ACKINIT_MSG_ID		9013*/
#define SINGLEMV_MSG_ID		9014
#define ASK4END_MSG_ID      9015
#define ANS4END_MSG_ID      9016

#include "smartBlocksBlockCode.h"
#include "smartBlocksSimulator.h"
#include "smartBlocksScheduler.h"
#include "smartBlocksBlock.h"
#include "smartBlocksCapabilities.h"

class MapMessage;
class AckMapMessage;
class SearchHeadMessage;
class SearchBackHeadMessage;
class SearchEndTrainMessage;
class TrainReadyMessage;
class CreateLineMessage;
class SetRDVMessage;
class UnlockMessage;
class ReconnectTrainMessage;
/*class DisableTrainMessage;
class NoActivityMessage;
class ReInitMessage;
class AckInitMessage;*/
class SingleMoveMessage;
class Ask4EndMessage;
class Ans4EndMessage;

typedef boost::shared_ptr<MapMessage> MapMessage_ptr;
typedef boost::shared_ptr<AckMapMessage> AckMapMessage_ptr;
typedef boost::shared_ptr<SearchHeadMessage> SearchHeadMessage_ptr;
typedef boost::shared_ptr<SearchBackHeadMessage> SearchBackHeadMessage_ptr;
typedef boost::shared_ptr<SearchEndTrainMessage> SearchEndTrainMessage_ptr;
typedef boost::shared_ptr<TrainReadyMessage> TrainReadyMessage_ptr;
typedef boost::shared_ptr<CreateLineMessage> CreateLineMessage_ptr;
typedef boost::shared_ptr<SetRDVMessage> SetRDVMessage_ptr;
typedef boost::shared_ptr<UnlockMessage> UnlockMessage_ptr;
typedef boost::shared_ptr<ReconnectTrainMessage> ReconnectTrainMessage_ptr;
/*typedef boost::shared_ptr<DisableTrainMessage> DisableTrainMessage_ptr;
typedef boost::shared_ptr<NoActivityMessage> NoActivityMessage_ptr;
typedef boost::shared_ptr<ReInitMessage> ReInitMessage_ptr;
typedef boost::shared_ptr<AckInitMessage> AckInitMessage_ptr;*/
typedef boost::shared_ptr<SingleMoveMessage> SingleMoveMessage_ptr;
typedef boost::shared_ptr<Ask4EndMessage> Ask4EndMessage_ptr;
typedef boost::shared_ptr<Ans4EndMessage> Ans4EndMessage_ptr;

class SbReconfBlockCode : public SmartBlocks::SmartBlocksBlockCode {
	int gridSize[2];
	SmartBlocks::presence *targetGrid;
	SmartBlocks::PointCel posGrid;
	SmartBlocks::PresenceMatrix _pm;
	P2PNetworkInterface *block2Answer,*_next,*_previous;
	bool _isHead,_isEnd,_isHeadOfLine;
	bool _activity;
	int _currentMove,_nbreWellPlacedBlocks,_nbreGoalCells;//_currentStage;
	int _numPrev;
	int nbreOfWaitedAnswers;
	SmartBlocks::SmartBlocksBlock *block;
	SmartBlocks::SmartBlocksWorld *wrl;
	SmartBlocks::PointCel _motionDir;
	vector <SmartBlocks::Validation*> *possibleRules;
	bool tabSteps[4];
	MessagePtr tabMemorisedMessages[4];

	short *unlockPathTab;
	int unlockPathTabSize;
	int unlockMotionStep;
public:
	SmartBlocks::SmartBlocksScheduler *scheduler;
	SmartBlocks::SmartBlocksBlock *smartBlock;

	SbReconfBlockCode (SmartBlocks::SmartBlocksBlock *host);
	~SbReconfBlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);

	static SmartBlocks::SmartBlocksBlockCode *buildNewBlockCode( SmartBlocks::SmartBlocksBlock *host);

	void sendMapToNeighbors(P2PNetworkInterface *except);
	void sendAckMap(P2PNetworkInterface *p2p);
	void createBorder();

	void sendAsk4EndToNeighbors(P2PNetworkInterface *except);
	void sendAns4EndMessage(P2PNetworkInterface *p2p,int value);

    void sendSearchBackHeadMessage(P2PNetworkInterface *dest,P2PNetworkInterface *except=NULL);

	void getLocalTargetGrid(const SmartBlocks::PointCel &pos,SmartBlocks::PresenceMatrix &pm);

    void applyRules();
    void printRules();
    void setRulesColor();
	void step2(MessagePtr message);
	void step3(MessagePtr message);
	void reconnect(bool hasRule);
	void createLine(uint64_t t,bool hol);
	void sendNoActivity(SmartBlocks::NeighborDirection dir,int id);
	void sendInitToNeighbors(P2PNetworkInterface *except,int stage);
	void sendAckInit(P2PNetworkInterface *p2p);
	void init();
	bool testIsthmus(int dx,int dy);
	bool testIsthmusTail(int dx,int dy);
	P2PNetworkInterface *getBorderPreviousNeightbor(P2PNetworkInterface *next);
	P2PNetworkInterface *getBorderNextNeightbor();
	P2PNetworkInterface *getBorderPreviousNeightborNoWellPlaced(P2PNetworkInterface *next);
	P2PNetworkInterface *getBorderNextNeightborNoWellPlaced(P2PNetworkInterface *prev);
	P2PNetworkInterface *getBorderSinglePrevious();
	P2PNetworkInterface *getBorderNeighborById(int id);

	void prepareUnlock(const vector<short>&path,int step);
	void startMotion(uint64_t t,const SmartBlocks::PointCel &mv,int step,const vector<short>&path);
	void singleMotion(SmartBlocks::Motion *,SmartBlocks::Capability *capa);
};

class MapMessage : public Message {
public :
	int gridw,gridh;
	SmartBlocks::presence *targetGrid;
	int posx,posy;
	int nbreGoalCells;
	MapMessage(int,int,int,int,int,SmartBlocks::presence*);
	~MapMessage();
};

class AckMapMessage : public Message {
public :
	AckMapMessage();
	~AckMapMessage();
};

class SearchHeadMessage : public Message {
public :
	SearchHeadMessage();
	~SearchHeadMessage();
};

class SearchBackHeadMessage : public Message {
public :
    P2PNetworkInterface *exceptionBlock;
	SearchBackHeadMessage(P2PNetworkInterface *except=NULL);
	~SearchBackHeadMessage();
};

class SearchEndTrainMessage : public Message {
public :
    int num;
	SearchEndTrainMessage (int n);
	~SearchEndTrainMessage();
};

class TrainReadyMessage : public Message {
public :
	bool queueFound;
    TrainReadyMessage(bool qf);
	~TrainReadyMessage();
};

class CreateLineMessage : public Message {
public :
	uint64_t etime;
    CreateLineMessage(uint64_t t);
	~CreateLineMessage();
};

class SetRDVMessage : public Message {
public :
	uint64_t rdvTime;
	SmartBlocks::PointCel motionVector;
    SetRDVMessage(uint64_t t,const SmartBlocks::PointCel &);
	~SetRDVMessage();
};

class UnlockMessage : public Message {
public :
	int sz;
	short *tab;
	int step;
    UnlockMessage(short *,int,int);
	~UnlockMessage();
};

class ReconnectTrainMessage : public Message {
public :
	bool hasRule;
	ReconnectTrainMessage(bool r);
	~ReconnectTrainMessage();
};
/*
class DisableTrainMessage : public Message {
public :
	DisableTrainMessage();
	~DisableTrainMessage();
};

class NoActivityMessage : public Message {
public :
	int senderID;
	NoActivityMessage(int sid);
	~NoActivityMessage();
};

class ReInitMessage : public Message {
public :
	int stage;
	ReInitMessage(int s);
	~ReInitMessage();
};

class AckInitMessage : public Message {
public :
	AckInitMessage();
	~AckInitMessage();
};
*/

class SingleMoveMessage : public Message {
public :
	int sz;
	short *tab;
	uint64_t startTime;
	SmartBlocks::PointCel motionVector;
	vector<short>unlockPath;
	int step;

    SingleMoveMessage(short *,int,uint64_t,const SmartBlocks::PointCel &,const vector<short>&up,int);
	~SingleMoveMessage();
};

class Ask4EndMessage : public Message {
public :
    int currentMove;
	Ask4EndMessage(int);
	~Ask4EndMessage();
};

class Ans4EndMessage : public Message {
public :
    int nbreWellPlaced;
	Ans4EndMessage(int);
	~Ans4EndMessage();
};


#endif /* SBRECONFBLOCKCODE_H_ */
