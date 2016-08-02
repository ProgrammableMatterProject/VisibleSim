/*
 * BbCycleBlockCode.cpp
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <sstream>
#include "scheduler.h"
#include "network.h"
#include "bbCycleBlockCode.h"

#include "trace.h"
#include "qclock.h"

using namespace std;
using namespace BlinkyBlocks;

#define REALISTIC_CLOCK

#define COLOR_CHANGE_PERIOD_USEC (2*1000*1000)
#define SIMULATION_DURATION_USEC (10*60*1000*1000)

BbCycleBlockCode::BbCycleBlockCode(BlinkyBlocksBlock *host): BlinkyBlocksBlockCode(host) {
	OUTPUT << "BbCycleBlockCode constructor" << endl;
#ifdef REALISTIC_CLOCK
	DNoiseQClock* localClock = DNoiseQClock::createXMEGA_RTC_OSC1K_CRC(host->blockId);
	host->setClock(localClock);
#endif
}

BbCycleBlockCode::~BbCycleBlockCode() {
	OUTPUT << "BbCycleBlockCode destructor" << endl;
}

void BbCycleBlockCode::init() {
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	stringstream info;
	
	uint64_t time = 0;
	while (time<SIMULATION_DURATION_USEC) {
		uint64_t globalTime =  bb->getSimulationTime(time);
		Color c = getColor(time/COLOR_CHANGE_PERIOD_USEC);
		getScheduler()->schedule(new SetColorEvent(globalTime,bb,c));
		time += COLOR_CHANGE_PERIOD_USEC;
	}
}

void BbCycleBlockCode::startup() {
	stringstream info;
	info << "  Starting BbCycleBlockCode in block " << hostBlock->blockId;
	init();
}

void BbCycleBlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
	BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*) hostBlock;
	info.str("");
	
	OUTPUT << bb->blockId << " processLocalEvent: date: "<< BaseSimulator::getScheduler()->now() << " process event " << pev->getEventName() << "(" << pev->eventType << ")" << ", random number : " << pev->randomNumber << endl;

	switch (pev->eventType) {
		case EVENT_SET_COLOR:
			{
			Color color = (std::static_pointer_cast<SetColorEvent>(pev))->color;
			bb->setColor(color);
			info << "set color "<< color << endl;
			}
			break;
		default:
			ERRPUT << "*** ERROR *** : unknown local event" << endl;
			break;
		}
		getScheduler()->trace(info.str(),hostBlock->blockId);
}


Color BbCycleBlockCode::getColor(uint64_t time) {
	Color colors[6] = {RED,GREEN,YELLOW,BLUE,GREY,PINK};
	int c = time%6;
	return colors[c];
}

BlockCode* BbCycleBlockCode::buildNewBlockCode(BuildingBlock *host) {
  return(new BbCycleBlockCode((BlinkyBlocksBlock*)host));
}
