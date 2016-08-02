/*
 * Map04BlockCode.h
 *
 *  Created on: may 22 2015
 *      Author: Vincent
 */

#ifndef ROBOT02BLOCKCODE_H_
#define ROBOT02BLOCKCODE_H_

#define GO_MAP_ID		        100
#define END_MAP_ID				101

#define FRAME_ID				200

#define BUFFER_ACK_ID			300
#define BUFFER_NACK_ID			301

#define SIZE					5

#include "robotBlocksBlockCode.h"
#include "robotBlocksSimulator.h"

#include "robotBlocksBlock.h"

class GoMap;
typedef std::shared_ptr<GoMap> GoMap_ptr;
class EndMap;
typedef std::shared_ptr<EndMap> EndMap_ptr;
class Frame;
typedef std::shared_ptr<Frame> Frame_ptr;
class BufferAck;
typedef std::shared_ptr<BufferAck> BufferAck_ptr;
class BufferNAck;
typedef std::shared_ptr<BufferNAck> BufferNAck_ptr;

class Map04BlockCode : public RobotBlocks::RobotBlocksBlockCode {

	P2PNetworkInterface *toMaster;
	int x, y, z;

	int NbOfWaitEnd;
	int NbOfWaitFrame;
	int buffer[SIZE][4];
	int buffer_c;

	bool received;
	bool reachMaster;
    
public:

	Scheduler *scheduler;
	RobotBlocks::RobotBlocksBlock *robotBlock;

	Map04BlockCode (RobotBlocks::RobotBlocksBlock *host);
	~Map04BlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);

	static RobotBlocks::RobotBlocksBlockCode *buildNewBlockCode( RobotBlocks::RobotBlocksBlock *host);

	void SendGoMap(P2PNetworkInterface *except, int &x_, int &y_, int &z_);
	void SendEndMap(P2PNetworkInterface *send);
	void SendFrame(P2PNetworkInterface *send);
	void SendBufferAck(P2PNetworkInterface *send, int &buffer_s_);
	void SendBufferNAck(P2PNetworkInterface *send, int &buffer_s_);

};

class GoMap : public Message{
public :

	int blockId;
	int x,y,z;

	GoMap(int &blockId_, int &x_, int &y_, int &z_);
	~GoMap();

};

class EndMap : public Message{
public : 

	EndMap();
	~EndMap();

};

class BufferNAck : public Message{
public : 

	int buffer_s;

	BufferNAck(int &buffer_s_);
	~BufferNAck();

};

class BufferAck : public Message{
public : 

	int buffer_s;

	BufferAck(int &buffer_s_);
	~BufferAck();

};

class Frame : public Message{
public : 

	int blockId;
	int x,y,z;

	Frame(int &blockId_, int &x_, int &y_, int &z_);
	~Frame();

};
#endif