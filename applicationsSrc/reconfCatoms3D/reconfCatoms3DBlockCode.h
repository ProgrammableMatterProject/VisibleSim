/*
 * csgCatoms3DBlockCode.h
 *
 *  Created on: 06 august 2015
 *  Author: Thadeu
 */

#ifndef RECONFCATOMS3DBLOCKCODE_H_
#define RECONFCATOMS3DBLOCKCODE_H_

#define CSG_MSG_ID	9001
#define DISTANCE_MSG_ID	9002

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "events.h"
#include "csgUtils.h"
#include "target.h"

class CSG_message;

typedef std::shared_ptr<CSG_message> CSG_message_ptr;

class ReconfCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
	Scheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;
	ReconfCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~ReconfCatoms3DBlockCode();

    Vector3D myPosition; // has relative position from the master
    bool hasPosition; // flag position
    CsgUtils csgUtils;
    static CSGNode *csgRoot;


	void startup();
	void processLocalEvent(EventPtr pev);
    void createCSG();
    void sendCSGMessage();

	static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

class CSG_message : public Message {
    char *csgBuffer;
    int csgBufferSize;
    Vector3D position;
public :
	CSG_message(char *_csgBuffer, int _csgBufferSize, Vector3D position);
	~CSG_message();

	char* getCsgBuffer() { return csgBuffer; };
	int getCsgBufferSize() { return csgBufferSize; };
	Vector3D getPosition() { return position; };
};


#endif /* RECONFCATOMS3DBLOCKCODE_H_ */
