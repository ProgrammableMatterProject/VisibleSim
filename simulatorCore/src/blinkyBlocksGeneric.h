#ifndef _blinkyBlocksGeneric_h
#define _blinkyBlocksGeneric_h

#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksEvents.h"
#include "blinkyBlocksBlock.h"
#include "lattice.h"

#include <map>
#include <ostream>
#include <functional>

namespace BlinkyBlocks {

class GenericCodeBlock;

typedef std::function<void (GenericCodeBlock*,MessagePtr,P2PNetworkInterface*)> eventFunc;

class GenericCodeBlock : public BlinkyBlocksBlockCode {
protected:
    multimap<int,eventFunc> eventFuncMap;
	Scheduler *scheduler;
	BlinkyBlocksBlock *module;
	ConsoleStream console;

    GenericCodeBlock(BlinkyBlocksBlock *host):BlinkyBlocksBlockCode(host) {
        scheduler = getScheduler();
        module = (BlinkyBlocksBlock*)hostBlock;
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
