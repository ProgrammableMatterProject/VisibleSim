/*
 * csgCatoms3DBlockCode.h
 *
 *  Created on: 06 august 2015
 *  Author: Thadeu
 */

#ifndef RECONFCATOMS3DBLOCKCODE_H_
#define RECONFCATOMS3DBLOCKCODE_H_

#define CATOM_MSG_ID	9001

#include <queue>
#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "events.h"
#include "csgUtils.h"
#include "target.h"
#include "seed.h"

enum RECONF_STATUS { WAITING, READY };

class ReconfCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
    static CSGNode *csgRoot;
    static queue<int> catomQueue;
    static BoundingBox boundingBox;
    static Seed *root;

	Scheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;
	Catoms3D::Catoms3DWorld *world;
    CsgUtils csgUtils;
    Vector3D worldPosition;
	ReconfCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~ReconfCatoms3DBlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);
    Vector3D getWorldPosition(BoundingBox bb, Cell3DPosition gridPosition);
    void createCSG();
    void sendCSGMessage();
    void addNeighbors();
    void addLine();

	static BlockCode *buildNewBlockCode(BuildingBlock *host);
private:
    bool cellHasBlock(const Cell3DPosition &pos);
};

class Catom_message : public Message {
    Catom_message() {};
    ~Catom_message();
};
#endif /* RECONFCATOMS3DBLOCKCODE_H_ */
