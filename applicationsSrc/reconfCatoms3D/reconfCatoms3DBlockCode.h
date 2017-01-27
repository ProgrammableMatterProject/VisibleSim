/*
 * reconfCatoms3DBlockCode.cpp
 *
 *  Created on: 17 October 2016
 *  Author: Thadeu
 */

#ifndef RECONFCATOMS3DBLOCKCODE_H_
#define RECONFCATOMS3DBLOCKCODE_H_

#define NEW_CATOM_MSG_ID	9001
#define NEW_CATOM_RESPONSE_MSG_ID	9002
#define RIGHT_SIDE_COMPLETED_MSG_ID	9003
#define LEFT_SIDE_COMPLETED_MSG_ID	9004

#include <queue>
#include <set>
#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "events.h"
#include "csgUtils.h"
#include "target.h"
#include "seed.h"

enum RECONF_STATUS { WAITING, READY };

enum SIDE_COMPLETED { LEFT = 0, RIGHT = 6 };

class ReconfCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
    static CSGNode *csgRoot;
    static BoundingBox boundingBox;
    static Seed *root;

    set<int> lineSeeds;
	Scheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;
    CsgUtils csgUtils;
    Vector3D worldPosition;

    bool leftCompleted, rightCompleted;
	ReconfCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~ReconfCatoms3DBlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);
    Vector3D getWorldPosition(Cell3DPosition gridPosition);
    void createCSG();
    void sendCSGMessage();
    void addNeighbor(Cell3DPosition pos);
    void addNeighbors();
    void addNeighborsOnXAxis();
    void addNextLineNeighbor();
    void sendMessageCompletedSide(SIDE_COMPLETED side);
    void sendMessageToGetNeighborInformation();
    bool isSeed();

	static BlockCode *buildNewBlockCode(BuildingBlock *host);
private:
    bool cellHasBlock(const Cell3DPosition &pos);
};

class New_catom_message : public Message {
public:
    New_catom_message() { id = NEW_CATOM_MSG_ID; };
};

class New_catom_response_message : public Message {
public:
    set<int> lineSeeds;
    bool leftCompleted, rightCompleted;
    New_catom_response_message() { id = NEW_CATOM_RESPONSE_MSG_ID; };
};

class Right_side_completed_message : public Message {
public:
    set<int> seeds;
    Right_side_completed_message(set<int> s) : seeds(s) { id = RIGHT_SIDE_COMPLETED_MSG_ID; };
};

class Left_side_completed_message : public Message {
public:
    set<int> seeds;
    Left_side_completed_message(set<int> s) : seeds(s) { id = LEFT_SIDE_COMPLETED_MSG_ID; };
};

class Catom_message : public Message {
    Catom_message() {};
    ~Catom_message();
};
#endif /* RECONFCATOMS3DBLOCKCODE_H_ */
