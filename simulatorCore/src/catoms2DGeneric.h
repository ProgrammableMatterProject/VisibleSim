#ifndef _catoms2DGeneric_h
#define _catoms2DGeneric_h

#include "catoms2DBlockCode.h"
#include "catoms2DSimulator.h"
#include "catoms2DScheduler.h"
#include "catoms2DEvents.h"
#include "catoms2DBlock.h"
#include <map>
#include <ostream>

namespace Catoms2D {

class GenericCodeBlock;

typedef boost::function<void (GenericCodeBlock*,MessagePtr,P2PNetworkInterface*)> eventFunc;

const int nbreNeighborsMax=6;

class GenericCodeBlock : public Catoms2DBlockCode {
protected:
    multimap<int,eventFunc> eventFuncMap;
	Catoms2DScheduler *scheduler;
	Catoms2DBlock *module;
	ConsoleStream console;

    GenericCodeBlock(Catoms2DBlock *host):Catoms2DBlockCode(host) {
        scheduler = Catoms2D::getScheduler();
        module = (Catoms2DBlock*)hostBlock;
        console.setInfo(scheduler,hostBlock->blockId);
        addDebugAttributes(scheduler);
    };
    ~GenericCodeBlock() { eventFuncMap.clear(); };

    void processLocalEvent(EventPtr pev);
    void addMessageEventFunc(int type,eventFunc);
    int sendMessageToAllNeighbors(Message*,int t0,int dt,int nexcept,...);
    int sendMessage(Message*,P2PNetworkInterface *,int t0,int dt);
    int sendMessageToAllNeighbors(const char*,Message*,int t0,int dt,int nexcept,...);
    int sendMessage(const char*,Message*,P2PNetworkInterface *,int t0,int dt);

    void setColor(const Color &c) { module->setColor(c); };
};

}

#endif
