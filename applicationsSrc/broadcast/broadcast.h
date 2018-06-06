#ifndef RECONFCATOMS3DBLOCKCODE_H_
#define RECONFCATOMS3DBLOCKCODE_H_

#include "catoms3DBlockCode.h"

class Broadcast : public Catoms3D::Catoms3DBlockCode {
public:
	Scheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;
    bool broadcast;

	Broadcast(Catoms3D::Catoms3DBlock *host);
	~Broadcast();

	void startup();
	void processLocalEvent(EventPtr pev);

	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* RECONFCATOMS3DBLOCKCODE_H_ */
