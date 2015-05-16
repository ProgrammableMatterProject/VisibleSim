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
#include <SFML/Audio.hpp> 
#include "scheduler.h"
#include "network.h"
#include "musicPlayerBlockCode.h"
#include "musicPlayerEvents.h"
#include "trace.h"
#include <vector>

using namespace std;
using namespace BlinkyBlocks;

#define SYNC_PERIOD (1*1000*1000)
#define SAMPLE_RATE 44100
#define TEMPO 65 
#define AMPLITUDE 30000
#define SAMPLES_Q (const unsigned)((60./(float)TEMPO)*SAMPLE_RATE) //duration of a quarter note 
#define SAMPLES_DQ (SAMPLES_Q*1.5) //duration of a dotted quarter note
#define SAMPLES_H (SAMPLES_Q*2) //duration of a half-note
#define SAMPLES_E (SAMPLES_Q/2) //duration of an 8th note
#define SAMPLES_S (SAMPLES_Q/4) //duration of an 16th note
#define A 440.00
#define B 493.88
#define C 261.63
#define D 293.66
#define E 329.63
#define F 349.23
#define G 392.00

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
	assigned=false;
	back=false;
	if(hostBlock->blockId==1){
		idMessage=0;
		std::vector<Note> song(Score());
		sendSongToNeighbors(NULL,song);
		BlinkyBlocks::getScheduler()->schedule(new SynchronizeEvent(BlinkyBlocks::getScheduler()->now()+SYNC_PERIOD,hostBlock));
		info << "This block is the Master Block" << endl;
	}
	
	BlinkyBlocks::getScheduler()->schedule(new PlayNoteEvent(BlinkyBlocks::getScheduler()->now()+1.2*SYNC_PERIOD,hostBlock));
	toPlay.push_back(Note(0,0,4));	
	BlinkyBlocks::getScheduler()->trace(info.str(),hostBlock->blockId);
}

void MusicPlayerBlockCode::startup() {
	stringstream info;
	delay=0;
	info << " Starting MusicPlayerBlockCode in block " << hostBlock->blockId;
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
			int SAMPLES;
			int sleepDuration;
			switch(toPlay[0].timeDiv) {
				case 2:
				SAMPLES=SAMPLES_H;
				sleepDuration=(60000/TEMPO)*2;
				break;
				case 4:
				SAMPLES=SAMPLES_Q;
				sleepDuration=60000/TEMPO;
				break;
				case 6:
				SAMPLES=SAMPLES_DQ;
				sleepDuration=(60000/TEMPO)*1.5;
				break;
				case 8:
				SAMPLES=SAMPLES_E;
				sleepDuration=60000/(2*TEMPO);
				break;
				case 16:
				SAMPLES=SAMPLES_S;
				sleepDuration=60000/(4*TEMPO);
				break;
				default:
				break;
			}
			sf::Int16 raw[SAMPLES];
			const double TWO_PI = 6.28318;
			double increment = toPlay.at(0).frequency/44100;
			double x = 0;
			float rel=toPlay.at(0).startTime;

			for (unsigned i = 0; i < SAMPLES; i++) {
			    raw[i] = AMPLITUDE * sin(x*TWO_PI);
			    x += increment;
			}
			sf::SoundBuffer buffer;	
	 		if (!buffer.loadFromSamples(raw, SAMPLES_Q, 1, SAMPLE_RATE)) {
  				info << "Loading failed!" << endl;
			}
			toPlay.erase(toPlay.begin());
			if (!toPlay.empty()){
				BlinkyBlocks::getScheduler()->schedule(new PlayNoteEvent(bb->getTime()+delay+(60000/TEMPO)*float((toPlay.at(0).startTime-rel)*1000),bb));
				info<<"Note scheduled at: "<<toPlay.at(0).startTime-rel<<endl;
			}
			sf::Sound Sound;
			Sound.setBuffer(buffer);
			Sound.play();
			info<<"note played"<<endl;
			sf::sleep(sf::milliseconds(sleepDuration));//We wait for the note to end 
			Sound.resetBuffer();
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
						//info<<"synchronized with delay : "<< delay << endl;
						}
					}
					break;
				case SCORE_MSG_ID:
					{
					ScoreMessage_ptr recvMessage = boost::static_pointer_cast<ScoreMessage>(message);
					block2Answer=recvInterface;
					if(!assigned){
						toPlay.push_back(recvMessage->score.at(0)); //We pick the first note, then erase it
						recvMessage->score.erase(recvMessage->score.begin());
						for (std::vector<Note>::iterator it = recvMessage->score.begin() ; it!=recvMessage->score.end() ; ++it){
							if(it->frequency==toPlay[1].frequency){ //as 0 is an empty Note to initialize
								toPlay.push_back(*it);
								recvMessage->score.erase(it);
								it--;
							}
						}
						assigned=true;
					}
					if (!recvMessage->score.empty())
						sendSongToNeighbors(block2Answer,recvMessage->score);
	
					info<<"Note assigned : "<<toPlay[1].frequency<<endl;
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
	ScoreMessage *message = new ScoreMessage(score);
	
	for (int i=0; i<6; i++){
	p2p = bb->getInterface(NeighborDirection::Direction(i));

		if (p2p->connectedInterface && p2p!=p2pExcept && !b[i] && !sent){
			BlinkyBlocks::getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent (BlinkyBlocks::getScheduler()->now(), message, p2p));
			sent=true;
			b[i]=true;
		}

		if(p2p==p2pExcept && !back){
			rootInterface=i;
			b[rootInterface]=true;
			back=true;
		}
	}

	if(!sent && hostBlock->blockId!=1)
		BlinkyBlocks::getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent (BlinkyBlocks::getScheduler()->now(), message, bb->getInterface(NeighborDirection::Direction(rootInterface))));
}

std::vector<Note> MusicPlayerBlockCode::Score(){ //We can take a midi file and break it down or just fill the score manually
	std::vector<Note> score;
	/*score.push_back(Note(C,1,8));
	score.push_back(Note(C,1.5,8));
	score.push_back(Note(C,2,8));
	score.push_back(Note(D,2.5,8));
	score.push_back(Note(E,3,8)); 
	score.push_back(Note(D,3.5,8));
	score.push_back(Note(C,4,8));
	score.push_back(Note(E,4.5,8)); 
	score.push_back(Note(D,5,8));
	score.push_back(Note(D,5.5,8));
	score.push_back(Note(C,6,8));*/
	
	score.push_back(Note(C,1,6));
	score.push_back(Note(B,2.5,16));
	score.push_back(Note(C,2.75,16));
	score.push_back(Note(D,3,16));
	score.push_back(Note(C,3.25,16));
	score.push_back(Note(B,3.5,16));
	score.push_back(Note(A,3.75,16));
	score.push_back(Note(C,4,8));
	score.push_back(Note(C,4.5,16));
	score.push_back(Note(A,4.75,16));
	score.push_back(Note(C,5,6));
	score.push_back(Note(B,6.5,16));
	score.push_back(Note(C,6.75,16));
	score.push_back(Note(A,7,16));
	score.push_back(Note(G,7.25,16));
	score.push_back(Note(E,7.5,16));
	score.push_back(Note(F,7.75,16));
	score.push_back(Note(G,8,2));
	score.push_back(Note(0,10,16));
	score.push_back(Note(F,10.25,16));
	score.push_back(Note(E,10.5,16));
	score.push_back(Note(D,10.75,16));
	score.push_back(Note(E,11,16));
	score.push_back(Note(F,11.25,16));
	score.push_back(Note(G,11.5,16));
	score.push_back(Note(A,11.75,16));
	score.push_back(Note(G,12,2));
	score.push_back(Note(0,14,16));
	score.push_back(Note(A,14.25,16));
	score.push_back(Note(B,14.5,16));
	score.push_back(Note(A,14.75,16));
	score.push_back(Note(G,15,16));
	score.push_back(Note(F,15.25,16));
	score.push_back(Note(E,15.5,16));
	score.push_back(Note(D,15.75,16));
	score.push_back(Note(E,16,16));
	score.push_back(Note(D,16.25,16));
	score.push_back(Note(C,16.5,8));
	score.push_back(Note(C,17.5,16));
	score.push_back(Note(D,17.75,16));
	score.push_back(Note(E,18,8));
	score.push_back(Note(F,18.5,8));
	score.push_back(Note(D,19,4));
	score.push_back(Note(G,20,4));
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

Note::Note(float freq, float time, int div){
	frequency=freq;
	startTime=time;
	timeDiv=div;
}

Note::~Note(){}
