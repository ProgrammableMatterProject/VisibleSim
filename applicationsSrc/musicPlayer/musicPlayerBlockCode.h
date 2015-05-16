/*
 *bbCycleBlockCode.h
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#ifndef MUSICPLAYERBLOCKCODE_H_
#define MUSICPLAYERBLOCKCODE_H_

#define SYNC_MSG_ID	9002
#define SCORE_MSG_ID	9001

#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksSimulator.h"
#include <boost/random.hpp>
#include "color.h"
#include <vector>

class SynchroMessage;
class ScoreMessage;
class Note;
typedef boost::shared_ptr<SynchroMessage> SynchroMessage_ptr;
typedef boost::shared_ptr<ScoreMessage> ScoreMessage_ptr;

class MusicPlayerBlockCode : public BlinkyBlocks::BlinkyBlocksBlockCode {
	P2PNetworkInterface *block2Answer;
	bool received[1000];
	bool b[6];
	bool assigned;
	bool back;
	int rootInterface;
	int64_t delay;
	int idMessage;
	std::vector<Note> toPlay;

public:
	MusicPlayerBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
	~MusicPlayerBlockCode();

	void startup();
	void init();
	void processLocalEvent(EventPtr pev);
	Color getColor(uint64_t time);
	std::vector<Note> Score();	
	void sendClockToNeighbors (P2PNetworkInterface *except, int hop, uint64_t clock, int id);	
	void sendSongToNeighbors (P2PNetworkInterface *except, std::vector<Note> score);
	static BlinkyBlocks::BlinkyBlocksBlockCode *buildNewBlockCode(BlinkyBlocks::BlinkyBlocksBlock *host);
};

class SynchroMessage : public Message {
public:
	int idSync;
	uint64_t time;
	int nbhop;
	SynchroMessage(uint64_t t, int hop, int id);
	unsigned int size() { return(17); }
	~SynchroMessage();
};

class ScoreMessage : public Message {
public:
	std::vector<Note> score;
	ScoreMessage(std::vector<Note> song);
	~ScoreMessage();
};

class Note {
public:
	float frequency;
	float startTime;
	int timeDiv;
	Note(float freq, float time, int div);
	~Note();
};
#endif /* BBCYCLEBLOCKCODE_H_ */
