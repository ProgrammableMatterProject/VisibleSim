/*
 * sphereCatoms3DBlockCode.h
 *
 *  Created on: 03 May 2016
 *  Author: Thadeu
 */

#ifndef SPHERECATOMS3DBLOCKCODE_H_
#define SPHERECATOMS3DBLOCKCODE_H_

#define SPHERE_MSG_ID	9001
#define DISTANCE_MSG_ID	9002

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DScheduler.h"
#include "catoms3DEvents.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "events.h"

class Distance_message;

typedef boost::shared_ptr<Distance_message> Distance_message_ptr;

class SphereCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
    static int radius[1000000];
	Catoms3D::Catoms3DScheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;
    int distance;

	SphereCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~SphereCatoms3DBlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);
    void sendDistanceMessage();

	static Catoms3D::Catoms3DBlockCode *buildNewBlockCode(Catoms3D::Catoms3DBlock *host);

};

class Distance_message : public Message {
    int distance;
public :
    Distance_message(int _dist);
	int getDistance() { return distance; };
};

class ACK_message : public Message {
    bool ok;
public :
    ACK_message(bool ok);
};

#endif /* SPHERECATOMS3DBLOCKCODE_H_ */
