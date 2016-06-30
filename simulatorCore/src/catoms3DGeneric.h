#ifndef _catoms3DGeneric_h
#define _catoms3DGeneric_h

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DEvents.h"
#include "catoms3DBlock.h"

#include <map>
#include <ostream>
#include <functional>

namespace Catoms3D {

class GenericCodeBlock;

typedef std::function<void (GenericCodeBlock*,MessagePtr,P2PNetworkInterface*)> eventFunc;

class GenericCodeBlock : public Catoms3DBlockCode {
protected:
    multimap<int,eventFunc> eventFuncMap;
	Scheduler *scheduler;
	Catoms3DBlock *module;
	ConsoleStream console;

    GenericCodeBlock(Catoms3DBlock *host):Catoms3DBlockCode(host) {
        scheduler = getScheduler();
        module = (Catoms3DBlock*)hostBlock;
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
