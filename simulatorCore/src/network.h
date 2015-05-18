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
#include <boost/shared_ptr.hpp>
#include "buildingBlock.h"

using namespace std;

class Message;
class P2PNetworkInterface;

typedef boost::shared_ptr<Message> MessagePtr;

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
	static unsigned int nextId;
	static unsigned int nbMessages;
public:
	unsigned int id;
	unsigned int type;
	P2PNetworkInterface *sourceInterface, *destinationInterface;

	Message();
	virtual ~Message();

	static unsigned int getNbMessages();
	virtual string getMessageName();

	virtual unsigned int size() { return(4); }
};

//===========================================================================================================
//
//          P2PNetworkInterface  (class)
//
//===========================================================================================================

class P2PNetworkInterface {
protected:
	static unsigned int nextId;
	static double defaultDataRate;
	static double defaultDataRateVariability;
	double dataRate; // bit/s
	double dataRateVariability;
	boost::rand48 generator;
public:
	
	unsigned int globalId;
	unsigned int localId;
	deque<MessagePtr> outgoingQueue;

	P2PNetworkInterface *connectedInterface;
	BaseSimulator::BuildingBlock *hostBlock;
	uint64_t availabilityDate;

	MessagePtr messageBeingTransmitted;

	P2PNetworkInterface(BaseSimulator::BuildingBlock *b);
	~P2PNetworkInterface();
	
	void send(Message *m);
	
	bool addToOutgoingBuffer(MessagePtr msg);
	void send();
	void connect(P2PNetworkInterface *ni);
	
	/*
	void disconnect();
	static void setDefaultDataRate(unsigned int rate) { defaultDataRate = rate; }
	*/
	
	void setDataRate(unsigned int rate) { dataRate = rate; }
	void setDataRateVariability(unsigned int variability) { dataRateVariability = variability; }
};

#endif /* NETWORK_H_ */
