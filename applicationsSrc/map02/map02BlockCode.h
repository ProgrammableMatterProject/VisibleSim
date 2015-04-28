/*
 * robot02BlockCode.h
 *
 *  Created on: 30 mars 2015
 *      Author: Vincent
 */

#ifndef ROBOT02BLOCKCODE_H_
#define ROBOT02BLOCKCODE_H_

#define GO_MAP_MSG_ID		        10001
#define END_MAP_MSG_ID		        10002
#define GO_FRAME_MSG_ID		        10011
#define END_FRAME_MSG_ID		    10012
#define SIZE						20000

#include "robotBlocksBlockCode.h"
#include "robotBlocksSimulator.h"
#include "robotBlocksScheduler.h"
#include "robotBlocksBlock.h"

class GoMapMessage;
class GoFrameMessage;
class EndMapMessage;

typedef boost::shared_ptr<GoMapMessage> GoMapMessage_ptr;
typedef boost::shared_ptr<GoFrameMessage> GoFrameMessage_ptr;

class Robot02BlockCode : public RobotBlocks::RobotBlocksBlockCode {

	P2PNetworkInterface *block2Answer;
	P2PNetworkInterface *pmid;
	Color myColor;
	int nbOfEnd;
	int nbOfFrame;
	int px,py,pz;
    bool recieved;
    bool lockEnd;
    int tab[SIZE][4];

public:

	RobotBlocks::RobotBlocksScheduler *scheduler;
	RobotBlocks::RobotBlocksBlock *robotBlock;

	Robot02BlockCode (RobotBlocks::RobotBlocksBlock *host);
	~Robot02BlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);

	static RobotBlocks::RobotBlocksBlockCode *buildNewBlockCode( RobotBlocks::RobotBlocksBlock *host);

	void SendGoMapMessage(P2PNetworkInterface *except, int &mid_, int &x_, int &y_, int &z_);
	void SendGoFrameMessage(P2PNetworkInterface *except, int &mid_, int &x_, int &y_, int &z_);
	void SendEndMapMessage(P2PNetworkInterface *send);
};

class GoFrameMessage : public Message {
public :

	int mid;
	int x;
	int y;
	int z;

	GoFrameMessage(int &mid_, int &x_, int &y_, int &z_);
	~GoFrameMessage();
};

class GoMapMessage : public Message {
public :

	int mid;
	int x;
	int y;
	int z;

	GoMapMessage(int &mid_, int &x_, int &y_, int &z_);
	~GoMapMessage();
};

class EndMapMessage : public Message{
public :

	EndMapMessage();
	~EndMapMessage();
};

#endif /* ROBOT2BLOCKCODE_H_ */