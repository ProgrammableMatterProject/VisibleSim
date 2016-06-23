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
#define DATA_TO_STORE				1003
#define DATA_STORED 				1004
#define CT_STATUS	 				1005

#include "robotBlocksBlockCode.h"
#include "robotBlocksSimulator.h"

#include "robotBlocksBlock.h"

class RequestStorage;
class OkStorage;
class NOkStorage;
class DataToStore;
class DataStored;
class CTStatus;

typedef std::shared_ptr<RequestStorage> RequestStorage_ptr;
typedef std::shared_ptr<OkStorage> OkStorage_ptr;
typedef std::shared_ptr<NOkStorage> NOkStorage_ptr;
typedef std::shared_ptr<DataToStore> DataToStore_ptr;
typedef std::shared_ptr<DataStored> DataStored_ptr;
typedef std::shared_ptr<CTStatus> CTStatus_ptr;

class Map03BlockCode : public RobotBlocks::RobotBlocksBlockCode {

	P2PNetworkInterface *answer;

	Color myColor;
	char direction;
	int d;
	bool ct;
    bool canStore;
    
public:

	Scheduler *scheduler;
	RobotBlocks::RobotBlocksBlock *robotBlock;

	Map03BlockCode (RobotBlocks::RobotBlocksBlock *host);
	~Map03BlockCode ();

	void startup();
	void processLocalEvent(EventPtr pev);
	void SendRequestStorage(char &direction, int &applicantId_);
	void SendOkStorage(P2PNetworkInterface *send, int &applicantId_, int &canStoreId_);
	void SendNOkStorage(P2PNetworkInterface *send);
	void SendDataToStore(P2PNetworkInterface *send, int &canStoreId_, int &mid_, int &x_, int &y_, int &z_);
	void SendDataStored(P2PNetworkInterface *send);
	void SendCTStatus(P2PNetworkInterface *send);
	int CountNeighbor(P2PNetworkInterface *except);
	static RobotBlocks::RobotBlocksBlockCode *buildNewBlockCode( RobotBlocks::RobotBlocksBlock *host);

};

class RequestStorage : public Message{
public :
	int applicantId;

	RequestStorage(int &applicantId_);
	~RequestStorage();
};

class OkStorage : public Message{
public : 
	int applicantId;
	int canStoreId;

	OkStorage(int &applicantId_, int &canStoreId_);
	~OkStorage();
};

class DataToStore : public Message{
public : 
	int canStoreId;
	int mid, x, y, z;

	DataToStore(int &canStoreId_, int &mid_, int &x_, int &y_, int &z_);
	~DataToStore();
};

class NOkStorage : public Message{
public : 
	NOkStorage();
	~NOkStorage();
};

class CTStatus : public Message{
public : 

	bool ct;
	CTStatus(bool &ct_);
	~CTStatus();
};

class DataStored : public Message{
public : 
	DataStored();
	~DataStored();
};

#endif /* ROBOT2BLOCKCODE_H_ */