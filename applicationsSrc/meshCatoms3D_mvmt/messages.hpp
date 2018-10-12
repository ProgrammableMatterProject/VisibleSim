/**
 * @file   messages.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 13:47:20 2018
 * 
 * @brief  
 * 
 * 
 */


#ifndef MC3D_MESSAGES_H_
#define MC3D_MESSAGES_H_

#include "network.h"
#include "../meltSortGrow/pathHop.hpp"

#include "catoms3DMotionEngine.hpp"
#include "meshSpanningTree.hpp"

static const uint MSG_DELAY = 1;

static const uint C3DMENG_FP = 64;
static const uint C3DMENG_FPF = 65;
static const uint C3DMENG_FPI = 66;
static const uint C3DMENG_FPNF = 67;

class Catoms3DMotionEngineMessage : public HandleableMessage {
public:
    Catoms3DMotionEngineMessage() {};
    virtual ~Catoms3DMotionEngineMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual void handle(Catoms3DMotionEngine&) {};
};

class FindPathMessage : public Catoms3DMotionEngineMessage {
    Cell3DPosition destination;
public:
    FindPathMessage(const Cell3DPosition& _pos)
        : destination(_pos) {
        type = C3DMENG_FP;
    };

    virtual ~FindPathMessage() {};

    virtual void handle(Catoms3DMotionEngine&);
    virtual Message* clone() { return new FindPathMessage(*this); }
    virtual string getName() { return "FindPath"; }
};

class FindPathFoundMessage : public Catoms3DMotionEngineMessage {
    list<PathHop> path;
public:
    FindPathFoundMessage(const list<PathHop>& _path) : path(_path) {
        type = C3DMENG_FPF;
    };

    virtual ~FindPathFoundMessage() {};

    virtual void handle(Catoms3DMotionEngine&);
    virtual Message* clone() { return new FindPathFoundMessage(*this); }
    virtual string getName() { return "FindPathFound"; }
};

class FindPathIgnoreMessage : public Catoms3DMotionEngineMessage {
public:
    FindPathIgnoreMessage() {
        type = C3DMENG_FPI;
    };
    
    virtual ~FindPathIgnoreMessage() {};

    virtual void handle(Catoms3DMotionEngine&);
    virtual Message* clone() { return new FindPathIgnoreMessage(*this); }
    virtual string getName() { return "FindPathIgnore"; }
};

class FindPathNotFoundMessage : public Catoms3DMotionEngineMessage {
public:
    FindPathNotFoundMessage() {
        type = C3DMENG_FPNF;
    };
    
    virtual ~FindPathNotFoundMessage() {};

    virtual void handle(Catoms3DMotionEngine&);
    virtual Message* clone() { return new FindPathNotFoundMessage(*this); }
    virtual string getName() { return "FindPathNotFound"; }
};

using namespace MeshSpanningTree;

class DisassemblyTriggerMessage : public AbstractMeshSpanningTreeMessage {
public:
    DisassemblyTriggerMessage(const MeshSpanningTreeRuleMatcher& _ruleMatcher,
                              const bool isAck)
        : AbstractMeshSpanningTreeMessage(_ruleMatcher, isAck) {};
    virtual ~DisassemblyTriggerMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new DisassemblyTriggerMessage(*this); }
    virtual string getName() { return "DisassemblyTrigger"; }
    virtual AbstractMeshSpanningTreeMessage*
    buildNewMeshSpanningTreeMessage(BaseSimulator::BlockCode& bc,
                                    const bool isAck) override;
};

class SubTreeScaffoldConstructionDoneMessage : public AbstractMeshSpanningTreeMessage {
public:
    SubTreeScaffoldConstructionDoneMessage(const MeshSpanningTreeRuleMatcher& _ruleMatcher,
                                           const bool isAck)
        : AbstractMeshSpanningTreeMessage(_ruleMatcher, true) {};
    virtual ~SubTreeScaffoldConstructionDoneMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new SubTreeScaffoldConstructionDoneMessage(*this); }
    virtual string getName() { return "SubTreeScaffoldConstructionDone"; }
    virtual AbstractMeshSpanningTreeMessage*
    buildNewMeshSpanningTreeMessage(BaseSimulator::BlockCode& bc,
                                    const bool isAck) override;
};

#endif /* MC3D_MESSAGES_H_ */
