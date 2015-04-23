/*
 * BbCycleBlockCode.cpp
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <sstream>
#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>
#include <stdio.h> 
#include "scheduler.h"
#include "network.h"
#include "musicPlayerBlockCode.h"
#include "musicPlayerEvents.h"
#include "trace.h"
#include <vector>

using namespace std;
using namespace BlinkyBlocks;

#define SYNC_PERIOD (2*1000*1000)
#define COLOR_CHANGE_PERIOD_USEC (2*1000*1000)
#define SIMULATION_DURATION_USEC (10*60*1000*1000)

MusicPlayerBlockCode::MusicPlayerBlockCode(BlinkyBlocksBlock *host): BlinkyBlocksBlockCode(host) {
	OUTPUT << "musicPlayerBlockCode constructor" << endl;
}

MusicPlayerBlockCode::~MusicPlayerBlockCode() {
	OUTPUT << "musicPlayerBlockCode destructor" << endl;
}

void MusicPlayerBlockCode::init() {
	stringstream info;
	block2Answer=NULL;
	b[6]={false};
	if(hostBlock->blockId==1){
		idMessage=0;
		std::vector<Note> song(Score());
		sendSongToNeighbors(NULL,song);
		BlinkyBlocks::getScheduler()->schedule(new SynchronizeEvent(BlinkyBlocks::getScheduler()->now()+SYNC_PERIOD,hostBlock));	
		info << "This block is the Master Block" << endl;
	}
	BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
}

void MusicPlayerBlockCode::startup() {
	stringstream info;
	delay=0;
	info << "  Starting MusicPlayerBlockCode in block " << hostBlock->blockId;
	init();
}

void MusicPlayerBlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
	MessagePtr message;
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	info.str("");
	
	OUTPUT << bb->blockId << " processLocalEvent: date: "<< BaseSimulator::getScheduler()->now() << " process event " << pev->getEventName() << "(" << pev->eventType << ")" << ", random number : " << pev->randomNumber << endl;
	
	switch (pev->eventType) {
		case EVENT_PLAY_NOTE:
			{

			}
			break;
		case EVENT_NI_RECEIVE:
			{
			message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message; 
			P2PNetworkInterface * recvInterface = message->destinationInterface;
			switch(message->id){
				case SYNC_MSG_ID: 
					{
					SynchroMessage_ptr recvMessage = boost::static_pointer_cast<SynchroMessage>(message);
					if (!received[recvMessage->idSync]){//If the block didn't already received the sync message of the wave, it synchronizes 
						received[recvMessage->idSync]=true;
						block2Answer=recvInterface;
						sendClockToNeighbors(block2Answer,recvMessage->nbhop+1,recvMessage->time,recvMessage->idSync); 	
						delay = recvMessage->time - bb->getTime() + 6000*recvMessage->nbhop;
						info<<"synchronized with delay : "<< delay << endl;
						}
					}
					break;
				case SCORE_MSG_ID:
					{
					ScoreMessage_ptr recvMessage = boost::static_pointer_cast<ScoreMessage>(message);
					block2Answer=recvInterface;
					toPlay.push_back(recvMessage->score.at(0)); //We pick the first note, then erase it
					recvMessage->score.erase(recvMessage->score.begin());
					for (int i=0; i!=recvMessage->score.size(); ++i){ 
						if (recvMessage->score[i].frequency == toPlay.begin()->frequency){ //We pick every other notes with the same frequency
							toPlay.push_back(recvMessage->score[i]);
							recvMessage->score.erase(recvMessage->score.begin()+i);
						}			
					}
					if (!recvMessage->score.empty())
						sendSongToNeighbors(block2Answer,recvMessage->score);
	
					info<<"Note assigned to the block : "<<toPlay.begin()->frequency<<endl;
					}
					break;					
				default:
					break;
				}
			}
			break;
		case EVENT_SYNC:
			{
			received[idMessage]=true;
			sendClockToNeighbors(NULL,1,bb->getTime(),idMessage);
			idMessage++;
			uint64_t nextSync = bb->getTime()+SYNC_PERIOD;
			BlinkyBlocks::getScheduler()->schedule(new SynchronizeEvent(nextSync,bb));
			info << "scheduled synchro" << endl;
			}
			break;
		default:
			ERRPUT << "*** ERROR *** : unknown local event" << endl;
			break;
		}
		BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
}

BlinkyBlocks::BlinkyBlocksBlockCode* MusicPlayerBlockCode::buildNewBlockCode(BlinkyBlocksBlock *host) {
	return(new MusicPlayerBlockCode(host));
}

void MusicPlayerBlockCode::sendClockToNeighbors (P2PNetworkInterface *p2pExcept, int hop, uint64_t clock, int id){
	P2PNetworkInterface * p2p;
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	
	for (int i=0; i<6 ; i++) {
	p2p = bb->getInterface(NeighborDirection::Direction(i));
		if (p2p->connectedInterface && p2p!=p2pExcept){	
			SynchroMessage *message = new SynchroMessage(clock, hop, id);
			BlinkyBlocks::getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent (BlinkyBlocks::getScheduler()->now(), message, p2p));
		}
	}
}

void MusicPlayerBlockCode::sendSongToNeighbors (P2PNetworkInterface *p2pExcept, std::vector<Note> score){
	P2PNetworkInterface * p2p;
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	bool sent=false;

	for (int i=0; i<6; i++){
	p2p = bb->getInterface(NeighborDirection::Direction(i));
		if (p2p->connectedInterface && p2p!=p2pExcept && !b[i]){
			ScoreMessage *message = new ScoreMessage(score);
			BlinkyBlocks::getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent (BlinkyBlocks::getScheduler()->now(), message, p2p));
			sent=true;
			b[i]=true;
			break;
		}
	}
		if (!sent){
			ScoreMessage *message = new ScoreMessage(score);
			BlinkyBlocks::getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent (BlinkyBlocks::getScheduler()->now(), message, p2pExcept));
		}	
}

std::vector<Note> MusicPlayerBlockCode::Score(){ //We can take a midi file and break it down or just fill the score manually
	std::vector<Note> score;
	score.push_back(Note(1000,503.25,2000));
	score.push_back(Note(1000,513.25,2000));
	score.push_back(Note(1000,523.25,2000));
	score.push_back(Note(1000,533.25,2000));
	score.push_back(Note(1000,543.25,2000));
	score.push_back(Note(1000,553.25,2000));
	score.push_back(Note(1000,563.25,2000));
	score.push_back(Note(1000,573.25,2000));
	score.push_back(Note(1000,583.25,2000));
	score.push_back(Note(1000,593.25,2000));
	score.push_back(Note(1000,5003.25,2000));
	score.push_back(Note(2500,5200.25,3500));
	score.push_back(Note(4000,587.33,5000));
	score.push_back(Note(5500,659.26,6500)); 
	return score;
}

SynchroMessage::SynchroMessage(uint64_t t, int hop, int ids) :Message(){
	id = SYNC_MSG_ID;
	idSync=ids;
	time = t;
	nbhop = hop;
}

SynchroMessage::~SynchroMessage(){}

ScoreMessage::ScoreMessage(std::vector<Note> song) :Message(){
	id = SCORE_MSG_ID;
	score=song;
}

ScoreMessage::~ScoreMessage(){}

Note::Note(int sTime, unsigned int freq, unsigned int time){
	startTime=sTime;
	frequency=freq;
	duration=time;
}

Note::~Note(){}
