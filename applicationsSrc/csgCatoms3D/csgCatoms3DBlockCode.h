/*
 * csgCatoms3DBlockCode.h
 *
 *  Created on: 06 august 2015
 *  Author: Thadeu
 */

#ifndef CSGCATOMS3DBLOCKCODE_H_
#define CSGCATOMS3DBLOCKCODE_H_

#define CSG_MSG_ID	9001

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DScheduler.h"
#include "catoms3DEvents.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "events.h"
#include "csgUtils.h"
#include "stoyUtils.h"

class CSG_message;

typedef boost::shared_ptr<CSG_message> CSG_message_ptr;

class CsgCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
	Catoms3D::Catoms3DScheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;
    Vecteur myPosition; // has relative position from the master
    bool hasPosition; // already has his position
    CsgUtils csgUtils;
    StoyUtils stoyUtils;

	CsgCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~CsgCatoms3DBlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);
    void createCSG();
    void sendCSGMessage();

	static Catoms3D::Catoms3DBlockCode *buildNewBlockCode(Catoms3D::Catoms3DBlock *host);

};

class CSG_message : public Message {
    char *csgBuffer;
    int csgBufferSize;
    Vecteur position;
    vector<Brick> bricks;
public :
	CSG_message(char *_csgBuffer, int _csgBufferSize, vector<Brick> bricks, Vecteur position);
	~CSG_message();

	char* getCsgBuffer() { return csgBuffer; };
	int getCsgBufferSize() { return csgBufferSize; };
	vector<Brick> getBricks() { return bricks; };
	Vecteur getPosition() { return position; };
};


#endif /* CSGCATOMS3DBLOCKCODE_H_ */
