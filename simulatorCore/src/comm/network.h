/*
 * network.h
 *
 *  Created on: 24 mars 2013
 *      Author: dom
 */

#ifndef NETWORK_H_
#define NETWORK_H_

#include <deque>
#include <string>

#include "rate.h"
#include "../utils/tDefs.h"
#include "../base/buildingBlock.h"

using namespace std;

class Message;
class P2PNetworkInterface;

typedef std::shared_ptr<Message> MessagePtr;

#ifdef DEBUG_OBJECT_LIFECYCLE
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
    //unsigned int id;
    unsigned int type;
    P2PNetworkInterface *sourceInterface, *destinationInterface;

    Message();
    Message(unsigned int t):type(t) {};
    virtual ~Message();

    static uint64_t getNbMessages();
    virtual string getMessageName() const;
    static void incrementMessageCounts() { nextId++; nbMessages++; }
    static inline void adjustClonedMessageCount() { nbMessages++; }

    virtual unsigned int size() const { return(4); }
    /**
     * @brief Clones the message. This is necessary when broadcasting
     * @attention Needs to overloaded in subclasses to avoid slicing (https://en.wikipedia.org/wiki/Object_slicing) when broadcasting subclasses of Message
     * @example virtual Message* clone() { return new MyMessageType(*this); }*/
    virtual Message* clone() const;
    virtual bool isMessageHandleable() const { return false; };
};

class HandleableMessage:public Message {
public:
    HandleableMessage();
    virtual ~HandleableMessage();

    virtual void handle(BaseSimulator::BlockCode*) = 0;
    virtual bool isMessageHandleable() const override { return true; };
    virtual string getMessageName() const override { return getName(); }; // TODO: factorize getName() with this
    virtual string getName() const = 0;
    virtual Message* clone() const override = 0;
};

template <class T>
class MessageOf:public Message {
    T *ptrData;
public :
    MessageOf(unsigned int t,const T &data):Message(t) { ptrData = new T(data); };
    ~MessageOf() { delete ptrData; };
    T* getData() const { return ptrData; };
    virtual Message* clone() const override {
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
    static uint64_t nextId;
    static int defaultDataRate;

    BaseSimulator::Rate* dataRate;
public:

    uint64_t globalId;
    uint64_t localId;
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
    bID getConnectedBlockBId() {
        return (connectedInterface!=NULL && connectedInterface->hostBlock!=NULL)?connectedInterface->hostBlock->blockId:0;
    }

    bool isConnected() const;

    void setDataRate(BaseSimulator::Rate* r);
    Time getTransmissionDuration(MessagePtr &m);
};

#endif /* NETWORK_H_ */
