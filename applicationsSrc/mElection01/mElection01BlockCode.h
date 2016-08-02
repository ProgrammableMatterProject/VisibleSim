/*
 * MElection01BlockCode.h
 *
 *  Created on: june 11 2015
 *      Author: Vincent
 */

#ifndef ROBOT02BLOCKCODE_H_
#define ROBOT02BLOCKCODE_H_

#define ID_DUFFUSION		        10
#define ID_ACK				        11
#define ID_NACK				        12

#define ID_BROADCAST				30
#define ID_BROADCAST_ANSWER			31

#include "robotBlocksBlockCode.h"
#include "robotBlocksSimulator.h"

#include "robotBlocksBlock.h"

class IdDiffusion;
typedef std::shared_ptr<IdDiffusion> IdDiffusion_ptr;
class IdAck;
typedef std::shared_ptr<IdAck> IdAck_ptr;
class IdNAck;
typedef std::shared_ptr<IdNAck> IdNAck_ptr;
class BroadcastID;
typedef std::shared_ptr<BroadcastID> BroadcastID_ptr;
class AnswerBroadcast;
typedef std::shared_ptr<AnswerBroadcast> AnswerBroadcast_ptr;

class MElection01BlockCode : public RobotBlocks::RobotBlocksBlockCode {

	int nbOfWaitDiffusion;
	int NbOfValidation;
	int bestId;
	int AmIMaster;
	bool lock;
	bool PMaster;
	P2PNetworkInterface *answer;
    
public:

	Scheduler *scheduler;
	RobotBlocks::RobotBlocksBlock *robotBlock;

	MElection01BlockCode (RobotBlocks::RobotBlocksBlock *host);
	~MElection01BlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);

	static RobotBlocks::RobotBlocksBlockCode *buildNewBlockCode( RobotBlocks::RobotBlocksBlock *host);
	void StartIdDiffusion(int &idBlock_);
	void SendIdAck(P2PNetworkInterface *send);
	void SendIdNAck(P2PNetworkInterface *send);
	int CountNeighbor(P2PNetworkInterface *except);
	void TellToNeighbors(int &bestId_);
	void SendBroadcastID(P2PNetworkInterface *except, int &bestId_);
	void SendAnswerBroadcast(P2PNetworkInterface *send, int &bestId_);

};

class IdDiffusion : public Message{
public:

	int idBlock;
	IdDiffusion(int &idBlock_);
	~IdDiffusion();
	
};

class IdAck : public Message{
public:

	IdAck();
	~IdAck();

};

class IdNAck : public Message{
public:

	IdNAck();
	~IdNAck();

};

class BroadcastID : public Message{
public:

	int bestId;
	BroadcastID(int &bestId_);
	~BroadcastID();

};

class AnswerBroadcast : public Message{
public:

	int bestId;
	AnswerBroadcast(int &bestId_);
	~AnswerBroadcast();

};

#endif