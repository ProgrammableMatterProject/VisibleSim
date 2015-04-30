/*
 * map03BlockCode.h
 *
 *  Created on: 30 avril 2015
 *      Author: Vincent
 */

#ifndef ROBOT02BLOCKCODE_H_
#define ROBOT02BLOCKCODE_H_

// Dans les request sotrage ID du demandeur : 
// l'ID du bloc qui envoi seulement quand on est 
// dans le boucle pas après sinon stocker dans les demandes
// -> Utile dans le cas d'une liste doublement chaînés 

#define REQUEST_STORAGE		        1000
#define OK_STORAGE			        1001
#define NOK_STORAGE					1002

#include "robotBlocksBlockCode.h"
#include "robotBlocksSimulator.h"
#include "robotBlocksScheduler.h"
#include "robotBlocksBlock.h"

class RequestStorage;

typedef boost::shared_ptr<RequestStorage> RequestStorage_ptr;

class Map03BlockCode : public RobotBlocks::RobotBlocksBlockCode {

	P2PNetworkInterface *block2Answer;

	Color myColor;
    bool canStore;
    
public:

	RobotBlocks::RobotBlocksScheduler *scheduler;
	RobotBlocks::RobotBlocksBlock *robotBlock;

	Map03BlockCode (RobotBlocks::RobotBlocksBlock *host);
	~Map03BlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);
	void SendRequestStorage(P2PNetworkInterface *send, int &applicantId_);
	static RobotBlocks::RobotBlocksBlockCode *buildNewBlockCode( RobotBlocks::RobotBlocksBlock *host);

};

class RequestStorage : public Message{
public :
	int applicantId;

	RequestStorage(int &applicantId_);
	~RequestStorage();
};

#endif /* ROBOT2BLOCKCODE_H_ */