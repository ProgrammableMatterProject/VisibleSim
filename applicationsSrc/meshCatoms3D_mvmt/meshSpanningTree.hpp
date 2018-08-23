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
    bool isOnXBranch(const Cell3DPosition& pos) const;
    bool isOnXBorder(const Cell3DPosition& pos) const;
    bool isOnYBranch(const Cell3DPosition& pos) const;
    bool isOnYBorder(const Cell3DPosition& pos) const;
    bool isOnZBranch(const Cell3DPosition& pos) const;
    bool isOnRevZBranch(const Cell3DPosition& pos) const;
    bool isOnMinus45DegZBranch(const Cell3DPosition& pos) const;
    bool isOnPlus45DegZBranch(const Cell3DPosition& pos) const;

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

    bool isInMesh(const Cell3DPosition& pos) const;

    void printDebugInfo(const Cell3DPosition& pos) const;
  
    /** 
     * @param pos position of the module to consider
     * @return the position of the parent module in the spanning tree, or pos if module has no parent
     */
    const Cell3DPosition getTreeParentPosition(const Cell3DPosition& pos) const;

    /** 
     * @param pos the module's position
     * @return the number of messages that should be received from the subtree before 
     *  propagating a response up the tree
     */
    unsigned int getNumberOfExpectedSubTreeConfirms(const Cell3DPosition& pos) const;
};

class AbstractMeshSpanningTreeMessage : public HandleableMessage {
protected:
    const MeshSpanningTreeRuleMatcher& ruleMatcher;
    const bool isAck;
public:
    AbstractMeshSpanningTreeMessage(const MeshSpanningTreeRuleMatcher& _ruleMatcher,
                                    const bool _isAck)
        : ruleMatcher(_ruleMatcher), isAck(_isAck) {};
    virtual ~AbstractMeshSpanningTreeMessage() {};

    virtual int forwardToNeighbors(BaseSimulator::BlockCode& bc,
                                   const P2PNetworkInterface* except_itf);

    virtual bool acknowledgeToParent(BaseSimulator::BlockCode& bc,
                                     P2PNetworkInterface* parent_itf);
    
    virtual AbstractMeshSpanningTreeMessage*
    buildNewMeshSpanningTreeMessage(BaseSimulator::BlockCode& bc, const bool isAck) = 0;
};  

}
#endif /* MESH_SPANNING_TREE_HPP_ */
