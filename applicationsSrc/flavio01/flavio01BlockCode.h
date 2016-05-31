/*
 * flavio01BlockCode.h
 *
 *  Created on: 26 mars 2013
 *      Author: dom
 */

#ifndef FLAVIO01BLOCKCODE_H_
#define FLAVIO01BLOCKCODE_H_

#include "multiCoresBlockCode.h"
#include "multiCoresSimulator.h"

class Flavio01BlockCode : public MultiCores::MultiCoresBlockCode {
public:
	bool computing;
	bool waitingForVM;

	Flavio01BlockCode(MultiCores::MultiCoresBlock *host);
	~Flavio01BlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);

	static MultiCores::MultiCoresBlockCode *buildNewBlockCode(MultiCores::MultiCoresBlock *host);
	//static MultiCoresBlockCode* buildNewBlockCode(MultiCoresBlock *host);
};


#endif /* FLAVIO01BLOCKCODE_H_ */
