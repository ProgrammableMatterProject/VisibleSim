/*
 * robot01BlockCode.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef ROBOT01BLOCKCODE_H_
#define ROBOT01BLOCKCODE_H_

#define MAP_MSG_ID		        9001
#define ACKMAP_MSG_ID	        9002
#define TRAIN_MSG_ID	        9003
#define ACKTRAIN_MSG_ID	      9004
#define PREPAREMOTION_MSG_ID	9005
#define MOTIONDELAY_MSG_ID	  9006
#define ANSWERDELAY_MSG_ID	  9007
#define UNLOCK_MSG_ID	        9008
#define TRAINREADY_MSG_ID	    9009
#define RUNTRAIN_MSG_ID	      9010
#define RELINKTRAIN_MSG_ID		9011

#include "robotBlocksBlockCode.h"
#include "robotBlocksSimulator.h"
#include "robotBlocksBlock.h"

class MapMessage;
class AckMapMessage;
class TrainMessage;
class AckTrainMessage;
class PrepareMotionMessage;
class MotionDelayMessage;
class AnswerDelayMessage;
class UnlockMessage;
class TrainReadyMessage;
class RunTrainMessage;
class ReLinkTrainMessage;

typedef std::shared_ptr<MapMessage> MapMessage_ptr;
typedef std::shared_ptr<AckMapMessage> AckMapMessage_ptr;
typedef std::shared_ptr<TrainMessage> TrainMessage_ptr;
typedef std::shared_ptr<AckTrainMessage> AckTrainMessage_ptr;
typedef std::shared_ptr<MotionDelayMessage> MotionDelayMessage_ptr;
typedef std::shared_ptr<AnswerDelayMessage> AnswerDelayMessage_ptr;
typedef std::shared_ptr<UnlockMessage> UnlockMessage_ptr;
typedef std::shared_ptr<ReLinkTrainMessage> ReLinkTrainMessage_ptr;
/*typedef std::shared_ptr<PrepareMotionMessage> PrepareMotionMessage_ptr;
typedef std::shared_ptr<TrainReadyMessage> TrainReadyMessage_ptr;
typedef std::shared_ptr<RunTrainMessage> RunTrainMessage_ptr;
*/

class Robot01BlockCode : public RobotBlocks::RobotBlocksBlockCode {
	short gridSize[3];
	RobotBlocks::presence *targetGrid;
	P2PNetworkInterface *block2Answer;
	int nbreOfWaitedAnswers,blockToUnlock;

  int currentTrainGain;
  RobotBlocks::PointRel3D currentTrainGoal;
	bool goodPlace;
	RobotBlocks::PointRel3D motionVector,nextMotionVector;
	P2PNetworkInterface *trainNext,*trainPrevious;
	vector <RobotBlocks::Validation*> *possibleMotions;
public:

    Scheduler *scheduler;
	RobotBlocks::RobotBlocksBlock *robotBlock;

	Robot01BlockCode (RobotBlocks::RobotBlocksBlock *host);
	~Robot01BlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);

	static RobotBlocks::RobotBlocksBlockCode *buildNewBlockCode( RobotBlocks::RobotBlocksBlock *host);

	void sendMapToNeighbors(P2PNetworkInterface *except);
	void sendAckMap(P2PNetworkInterface *p2p);
	void sendLinkTrainMessages(P2PNetworkInterface *sender);
	void getLocalTargetGrid(const RobotBlocks::PointRel3D &pos,RobotBlocks::PresenceMatrix &pm);
	void sendAnswerDelayOrMotionDelayMessage(uint64_t gt);
	void sendUnlockMessage(int id);
	void calcPossibleMotions(const RobotBlocks::PointRel3D &np);
	void sendReLinkTrainMessage();
/*	void sendTrainMessage(P2PNetworkInterface *p2p,const RobotBlocks::PointRel3D &motion,bool ar);
	void sendTrainReadyMessage(P2PNetworkInterface *p2p);
	void sendPrepareMotionMessageToNeighbors(P2PNetworkInterface *except);
	void sendRunTrainToNeighbors(P2PNetworkInterface *except);
	bool linkTrain(bool head,bool ar);*/
};

class MapMessage : public Message {
public :
	short gridSize[3];
	RobotBlocks::presence *targetGrid;
	MapMessage(short*,RobotBlocks::presence*);
	~MapMessage();
};

class AckMapMessage : public Message {
public :
	AckMapMessage();
	~AckMapMessage();
};

class TrainMessage : public Message {
public :
	RobotBlocks::PointRel3D newPos;
	int gain;
	TrainMessage(const RobotBlocks::PointRel3D &p,int g);
	~TrainMessage();
};

class AckTrainMessage : public Message {
public :
    bool answer;
	AckTrainMessage(bool v) ;
	~AckTrainMessage();
};

class MotionDelayMessage : public Message {
public :
    uint64_t globalTime;
    bool unlockMode;
    MotionDelayMessage(uint64_t time,bool unlock=false);
	~MotionDelayMessage();
};

class AnswerDelayMessage : public Message {
public :
    uint64_t globalRDVTime;
    bool block2unlock;
    AnswerDelayMessage(uint64_t time,bool b2ul);
	~AnswerDelayMessage();
};

class UnlockMessage : public Message {
public :
    int target;
    UnlockMessage(int id);
    ~UnlockMessage();
};

class ReLinkTrainMessage : public Message {
public :
		ReLinkTrainMessage();
    ~ReLinkTrainMessage();
};

#endif /* SBRECONFBLOCKCODE_H_ */
