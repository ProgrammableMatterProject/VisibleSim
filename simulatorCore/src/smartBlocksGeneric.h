#ifndef _smartBlocksGeneric_h
#define _smartBlocksGeneric_h

#include "smartBlocksBlockCode.h"
#include "smartBlocksSimulator.h"
#include "translationEvents.h"
#include "smartBlocksBlock.h"
#include "lattice.h"

#include <map>
#include <ostream>
#include <functional>

namespace SmartBlocks {

class GenericCodeBlock;

typedef std::function<void (GenericCodeBlock*,MessagePtr,P2PNetworkInterface*)> eventFunc;

class GenericCodeBlock : public SmartBlocksBlockCode {
protected:
    multimap<int,eventFunc> eventFuncMap;
	Scheduler *scheduler;
	SmartBlocksBlock *module;
	ConsoleStream console;

    GenericCodeBlock(SmartBlocksBlock *host):SmartBlocksBlockCode(host) {
        scheduler = getScheduler();
        module = (SmartBlocksBlock*)hostBlock;
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
