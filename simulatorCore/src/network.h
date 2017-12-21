/*
 * network.h
 *
 *  Created on: 24 mars 2013
 *      Author: dom
 */

#ifndef NETWORK_H_
#define NETWORK_H_

#include <deque>
#include <string.h>

#include "tDefs.h"
#include "rate.h"
#include "buildingBlock.h"

using namespace std;

class Message;
class P2PNetworkInterface;

typedef std::shared_ptr<Message> MessagePtr;

#ifdef DEBUG_MESSAGES
#define MESSAGE_CONSTRUCTOR_INFO()			(cout << getMessageName() << " constructor (" << id << ")" << endl)
#define MESSAGE_DESTRUCTOR_INFO()			(cout << getMessageName() << " destructor (" << id << ")" << endl)
#else
#define MESSAGE_CONSTRUCTOR_INFO()
#define MESSAGE_DESTRUCTOR_INFO()
#endif

//===========================================================================================================
//
//          Message  (class)
//
//===========================================================================================================

class Message {
protected:
	static uint64_t nextId;
	static uint64_t nbMessages;
public:
	uint64_t id;
	unsigned int type;
	P2PNetworkInterface *sourceInterface, *destinationInterface;

	Message();
	virtual ~Message();

	static uint64_t getNbMessages();
	virtual string getMessageName();

	virtual unsigned int size() { return(4); }
	virtual Message* clone();
    virtual bool isMessageHandeable() { return false; };
};

class HandleableMessage:public Message {
public:
	HandleableMessage();
	virtual ~HandleableMessage();

    virtual void handle(BaseSimulator::BlockCode*) = 0;

    virtual bool isMessageHandeable() { return true; };
};

template <class T>
class MessageOf:public Message {
    T *ptrData;
    public :
    MessageOf(int t,const T &data):Message() { type=t; ptrData = new T(data);};
    ~MessageOf() { delete ptrData; };
    T* getData() const { return ptrData; };
    virtual Message* clone() {
        MessageOf<T> *ptr = new MessageOf<T>(type,*ptrData);
        ptr->sourceInterface = sourceInterface;
        ptr->destinationInterface = destinationInterface;
        return ptr;
    }
};

//===========================================================================================================
//
//          P2PNetworkInterface  (class)
//
//===========================================================================================================

class P2PNetworkInterface {
protected:
	static unsigned int nextId;
	static int defaultDataRate;
	
	BaseSimulator::Rate* dataRate;
public:
	
	unsigned int globalId;
	unsigned int localId;
	deque<MessagePtr> outgoingQueue;

	P2PNetworkInterface *connectedInterface;
	BaseSimulator::BuildingBlock *hostBlock;
	Time availabilityDate;

	MessagePtr messageBeingTransmitted;

	P2PNetworkInterface(BaseSimulator::BuildingBlock *b);
	~P2PNetworkInterface();
	
	void send(Message *m);
	
	bool addToOutgoingBuffer(MessagePtr msg);
	void send();
	void connect(P2PNetworkInterface *ni);
	int getConnectedBlockId() {
        return (connectedInterface!=NULL && connectedInterface->hostBlock!=NULL)?connectedInterface->hostBlock->blockId:-1;
	}

	bool isConnected();
	
	void setDataRate(BaseSimulator::Rate* r); 
	Time getTransmissionDuration(MessagePtr &m);
};

#endif /* NETWORK_H_ */
