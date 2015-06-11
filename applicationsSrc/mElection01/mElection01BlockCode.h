/*
 * Map04BlockCode.h
 *
 *  Created on: may 22 2015
 *      Author: Vincent
 */

#ifndef ROBOT02BLOCKCODE_H_
#define ROBOT02BLOCKCODE_H_

#define ID_DUFFUSION		        10
#define ID_ACK				        11
#define ID_NACK				        12

#define ASK_NEIGHBOR				20
#define ANSWER_NEIGHBOR				21

#include "robotBlocksBlockCode.h"
#include "robotBlocksSimulator.h"
#include "robotBlocksScheduler.h"
#include "robotBlocksBlock.h"

class IdDiffusion;
typedef boost::shared_ptr<IdDiffusion> IdDiffusion_ptr;
class IdAck;
typedef boost::shared_ptr<IdAck> IdAck_ptr;
class IdNAck;
typedef boost::shared_ptr<IdNAck> IdNAck_ptr;


class MElection01BlockCode : public RobotBlocks::RobotBlocksBlockCode {

	int nbOfWaitDiffusion;
	int bestId;
	int AmIMaster;
	int lock;
	bool DoIBroadcast;
	P2PNetworkInterface *answer;
    
public:

	RobotBlocks::RobotBlocksScheduler *scheduler;
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

#endif