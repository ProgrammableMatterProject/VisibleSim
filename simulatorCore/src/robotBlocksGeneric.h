#ifndef _robotBlocksGeneric_h
#define _robotBlocksGeneric_h

#include "robotBlocksBlockCode.h"
#include "robotBlocksSimulator.h"
#include "translationEvents.h"
#include "robotBlocksBlock.h"
#include "lattice.h"

#include <map>
#include <ostream>
#include <functional>

namespace RobotBlocks {

class GenericCodeBlock;

typedef std::function<void (GenericCodeBlock*,MessagePtr,P2PNetworkInterface*)> eventFunc;

class GenericCodeBlock : public RobotBlocksBlockCode {
protected:
    multimap<int,eventFunc> eventFuncMap;
	Scheduler *scheduler;
	RobotBlocksBlock *module;
	ConsoleStream console;

    GenericCodeBlock(RobotBlocksBlock *host):RobotBlocksBlockCode(host) {
        scheduler = getScheduler();
        module = (RobotBlocksBlock*)hostBlock;
        console.setInfo(scheduler,hostBlock->blockId);
        addDebugAttributes(scheduler);
    };
    ~GenericCodeBlock() { eventFuncMap.clear(); };

    virtual void onTap();

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
