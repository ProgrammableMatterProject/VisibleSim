/*
 * blockCode.h
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#ifndef BLOCKCODE_H_
#define BLOCKCODE_H_

#include "buildingBlock.h"
#include <inttypes.h>
class Event;
typedef boost::shared_ptr<Event> EventPtr;

namespace BaseSimulator {

class BuildingBlock;

class BlockCode {
public:
	BuildingBlock *hostBlock;
	uint64_t availabilityDate;

	BlockCode(BuildingBlock *host);
	virtual ~BlockCode();

	virtual void processLocalEvent(EventPtr pev) = 0;
	virtual void startup() = 0;
};

} // BaseSimulator namespace

#endif /* BLOCKCODE_H_ */
