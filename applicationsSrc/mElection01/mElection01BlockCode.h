/*
 * Map04BlockCode.h
 *
 *  Created on: may 22 2015
 *      Author: Vincent
 */

#ifndef ROBOT02BLOCKCODE_H_
#define ROBOT02BLOCKCODE_H_

#define START_MASTER_ID		        10
#define START_ACK_ID		        11
#define START_NACK_ID		        12

#define ASK_NEIGHBOR				20
#define ANSWER_NEIGHBOR				21

#include "robotBlocksBlockCode.h"
#include "robotBlocksSimulator.h"
#include "robotBlocksScheduler.h"
#include "robotBlocksBlock.h"

class StartMaster;
typedef boost::shared_ptr<StartMaster> StartMaster_ptr;

class StartAckMaster;
typedef boost::shared_ptr<StartAckMaster> StartAckMaster_ptr;

class StartNAckMaster;
typedef boost::shared_ptr<StartNAckMaster> StartNAckMaster_ptr;

class AskNeighbor;
typedef boost::shared_ptr<AskNeighbor> AskNeighbor_ptr;

class AnswerNeighbor;
typedef boost::shared_ptr<AnswerNeighbor> AnswerNeighbor_ptr;

class MElection01BlockCode : public RobotBlocks::RobotBlocksBlockCode {

	int nbOfWaitStart;
	int bestId;
	int AmIMaster;
	int lock;
	P2PNetworkInterface *answer;
    
public:

	RobotBlocks::RobotBlocksScheduler *scheduler;
	RobotBlocks::RobotBlocksBlock *robotBlock;

	MElection01BlockCode (RobotBlocks::RobotBlocksBlock *host);
	~MElection01BlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);

	static RobotBlocks::RobotBlocksBlockCode *buildNewBlockCode( RobotBlocks::RobotBlocksBlock *host);
	void SendStartMaster();
	void SendStartAckMaster(P2PNetworkInterface *send);
	void SendStartNAckMaster(P2PNetworkInterface *send);
	int countNeighbors(P2PNetworkInterface *except);
	void SendAskNeighbor(int &applicantId_);
	void SendAnswerNeighbor(P2PNetworkInterface *send, int &applicantId_);

};

class StartMaster : public Message{
public :

	int blockId;
	StartMaster(int &blockId_);
	~StartMaster();

};

class StartAckMaster : public Message{
public : 

	StartAckMaster();
	~StartAckMaster();

};

class StartNAckMaster : public Message{
public : 

	StartNAckMaster();
	~StartNAckMaster();

};

class AskNeighbor : public Message{
public : 

	int applicantId;

	AskNeighbor(int &applicantId_);
	~AskNeighbor();

};

class AnswerNeighbor : public Message{
public : 

	int applicantId;
	AnswerNeighbor(int &applicantId_);
	~AnswerNeighbor();

};

#endif