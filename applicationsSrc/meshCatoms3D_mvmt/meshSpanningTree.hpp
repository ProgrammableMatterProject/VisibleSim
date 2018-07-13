/**
 * @file   meshSpanningTree.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Fri Jul 13 13:25:47 2018
 * 
 * @brief  
 * 
 * 
 */


#ifndef MESH_SPANNING_TREE_HPP_
#define MESH_SPANNING_TREE_HPP_

#include "network.h"
#include "cell3DPosition.h"

class MeshCatoms3DBlockCode;
namespace MeshSpanningTree {

static const uint MC_ST_A = 68;
static const uint MC_ST_B = 69;

class MeshSpanningTreeRuleMatcher {
    const uint X_MAX, Y_MAX, B;
public:
    MeshSpanningTreeRuleMatcher(const uint _X_MAX, const uint _Y_MAX, const uint _B) :
        X_MAX(_X_MAX), Y_MAX(_Y_MAX), B(_B) {};
    virtual ~MeshSpanningTreeRuleMatcher() {};
    
    bool isOnPartialBorderMesh(const Cell3DPosition& pos) const;

    bool isTileRoot(const Cell3DPosition& pos) const;

    bool upwardBranchRulesApply(const Cell3DPosition& own,
                                const Cell3DPosition& other) const;

    bool planarBranchRulesApply(const Cell3DPosition& own,
                                const Cell3DPosition& other) const;

    bool meshRootBranchRulesApply(const Cell3DPosition& own,
                                  const Cell3DPosition& other) const;

    bool partialBorderMeshRulesApply(const Cell3DPosition& own,
                                 const Cell3DPosition& other) const;

    bool shouldSendToNeighbor(const Cell3DPosition& own,
                              const Cell3DPosition& other) const;
};
    
class MeshSpanningTreeMessage : public HandleableMessage {
    const MeshSpanningTreeRuleMatcher& ruleMatcher;
public:
    MeshSpanningTreeMessage(const MeshSpanningTreeRuleMatcher& _ruleMatcher)
        : ruleMatcher(_ruleMatcher) {};
    virtual ~MeshSpanningTreeMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new MeshSpanningTreeMessage(*this); }
    virtual string getName() { return "MeshSpanningTree"; }
    bool forwardToNeighbors(MeshCatoms3DBlockCode&);
};

class MeshSpanningTreeAckMessage : public HandleableMessage {
    // const P2PNetworkInterface& parent;
public:
    // MeshSpanningTreeAckMessage(const P2PNetworkInterface& itf) : parent(itf) {};
    MeshSpanningTreeAckMessage() {};
    virtual ~MeshSpanningTreeAckMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new MeshSpanningTreeAckMessage(*this); }
    virtual string getName() { return "MeshSpanningTreeAck"; }
    bool acknowledgeToParent();
};

};
#endif /* MESH_SPANNING_TREE_HPP_ */
