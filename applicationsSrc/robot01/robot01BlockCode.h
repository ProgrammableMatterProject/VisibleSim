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
#include "capabilities.h"

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

static presence *targetGrid = NULL; //!< An array representing the target grid of the simulation, i.e. the shape to produce (can be 2D / 3D)
static Capabilities *capabilities; //!< The capabilities available for the blocks simulated in this world

class Robot01BlockCode : public RobotBlocks::RobotBlocksBlockCode {
	short gridSize[3];
	presence *targetGrid;
	P2PNetworkInterface *block2Answer;
	int nbreOfWaitedAnswers,blockToUnlock;

	int currentTrainGain;
	PointRel3D currentTrainGoal;
	bool goodPlace;
	PointRel3D motionVector,nextMotionVector;
	P2PNetworkInterface *trainNext,*trainPrevious;
	vector <Validation*> *possibleMotions;
	Lattice *lattice;
public:
    Scheduler *scheduler;
	RobotBlocks::RobotBlocksBlock *robotBlock;

	Robot01BlockCode (RobotBlocks::RobotBlocksBlock *host);
	~Robot01BlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);

	static BlockCode *buildNewBlockCode(BuildingBlock *host);
	
	void sendMapToNeighbors(P2PNetworkInterface *except);
	void sendAckMap(P2PNetworkInterface *p2p);
	void sendLinkTrainMessages(P2PNetworkInterface *sender);
	void getLocalTargetGrid(const PointRel3D &pos,PresenceMatrix &pm);
	void sendAnswerDelayOrMotionDelayMessage(uint64_t gt);
	void sendUnlockMessage(int id);
	void calcPossibleMotions(const PointRel3D &np);
	void sendReLinkTrainMessage();
/*	void sendTrainMessage(P2PNetworkInterface *p2p,const PointRel3D &motion,bool ar);
	void sendTrainReadyMessage(P2PNetworkInterface *p2p);
	void sendPrepareMotionMessageToNeighbors(P2PNetworkInterface *except);
	void sendRunTrainToNeighbors(P2PNetworkInterface *except);
	bool linkTrain(bool head,bool ar);*/

	inline presence *getTargetGridPtr(short *gs)
        { memcpy(gs,lattice->gridSize.pt,3*sizeof(short)); return targetGrid; };
    inline presence getTargetGrid(int ix,int iy,int iz)
        { return targetGrid[(iz*lattice->gridSize[1]+iy)*lattice->gridSize[0]+ix]; };
    inline void setTargetGrid(presence value,int ix,int iy,int iz)
        { targetGrid[(iz*lattice->gridSize[1]+iy)*lattice->gridSize[0]+ix]=value; };
    void initTargetGrid();
    inline void setCapabilities(Capabilities *capa) { capabilities=capa; };
    void getPresenceMatrix(const PointRel3D &pos,PresenceMatrix &pm);
    inline Capabilities* getCapabilities() { return capabilities; };

	virtual void parseUserElements(TiXmlDocument *config);
};

class MapMessage : public Message {
public :
	short gridSize[3];
	presence *targetGrid;
	MapMessage(short*,presence*);
	~MapMessage();
};

class AckMapMessage : public Message {
public :
	AckMapMessage();
	~AckMapMessage();
};

class TrainMessage : public Message {
public :
	PointRel3D newPos;
	int gain;
	TrainMessage(const PointRel3D &p,int g);
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
