/*
 * csgCatoms3DBlockCode.h
 *
 *  Created on: 06 august 2015
 *  Author: Thadeu
 */

#ifndef SIMPLECATOM3DBLOCKCODE_H_
#define SIMPLECATOM3DBLOCKCODE_H_

#define CSG_MSG_ID	9001

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DScheduler.h"
#include "catoms3DEvents.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "events.h"

class CSG_message;

typedef boost::shared_ptr<CSG_message> CSG_message_ptr;

class CsgCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
	Catoms3D::Catoms3DScheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;
    Vecteur myPosition;
    bool hasPosition;
    CsgNode csgTree;

	CsgCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~CsgCatoms3DBlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);
    void createCSG();
    void sendCSGMessage();
    bool isInCSG();
    bool isInCSG(CsgNode &node, Vecteur basePosition);

	static Catoms3D::Catoms3DBlockCode *buildNewBlockCode(Catoms3D::Catoms3DBlock *host);

};

class CSG_message : public Message {
	CsgNode csgTree; //TODO check dangling pointer here
    Vecteur position;

public :
	CSG_message(CsgNode, Vecteur position);
	~CSG_message();

	CsgNode getCsgTree() { return csgTree; };
	Vecteur getPosition() { return position; };
};


#endif /* SIMPLECATOM3DBLOCKCODE_H_ */
