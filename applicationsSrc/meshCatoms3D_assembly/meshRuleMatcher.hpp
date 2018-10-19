/**
 * @file   meshRuleMatcher.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Fri Jul 13 13:25:47 2018
 * 
 * @brief  
 * 
 * 
 */


#ifndef MESH_RULE_MATCHER_HPP_
#define MESH_RULE_MATCHER_HPP_

#include "network.h"
#include "cell3DPosition.h"
#include "catoms3DBlockCode.h"

#define MSG_DELAY_MC 5000

static const uint MC_ST_A = 68;
static const uint MC_ST_B = 69;

namespace MeshCoating {

enum BranchIndex { ZBranch, RevZBranch, LeftZBranch,
                       RightZBranch, XBranch, YBranch, N_BRANCHES };

class MeshRuleMatcher {
    const int X_MAX, Y_MAX, Z_MAX, B;

public:
    bool isOnXBranch(const Cell3DPosition& pos) const;
    bool isOnXBorder(const Cell3DPosition& pos) const;
    bool isOnXOppBorder(const Cell3DPosition& pos) const;
    bool isOnYBranch(const Cell3DPosition& pos) const;    
    bool isOnYBorder(const Cell3DPosition& pos) const;
    bool isOnYOppBorder(const Cell3DPosition& pos) const;
    bool isOnZBranch(const Cell3DPosition& pos) const;
    bool isOnRevZBranch(const Cell3DPosition& pos) const;
    bool isOnRightZBranch(const Cell3DPosition& pos) const;
    bool isOnLeftZBranch(const Cell3DPosition& pos) const;    
    
    bool shouldGrowZBranch(const Cell3DPosition& pos) const;
    bool shouldGrowRevZBranch(const Cell3DPosition& pos) const;
    bool shouldGrowLeftZBranch(const Cell3DPosition& pos) const;
    bool shouldGrowRightZBranch(const Cell3DPosition& pos) const;
    bool shouldGrowXBranch(const Cell3DPosition& pos) const;
    bool shouldGrowYBranch(const Cell3DPosition& pos) const;    

    Cell3DPosition getBranchUnitOffset(int bi);
    BranchIndex getBranchIndexForNonRootPosition(const Cell3DPosition& pos);
    
    MeshRuleMatcher(const uint _X_MAX, const uint _Y_MAX, const uint _Z_MAX,
                                const uint _B) :
        X_MAX(_X_MAX), Y_MAX(_Y_MAX), Z_MAX(_Z_MAX), B(_B) {};
    virtual ~MeshRuleMatcher() {};
    
    bool isInGrid(const Cell3DPosition& pos) const;
    bool isInMesh(const Cell3DPosition& pos) const;
    bool isOnPartialBorderMesh(const Cell3DPosition& pos) const;
    bool isTileRoot(const Cell3DPosition& pos) const;
    bool isVerticalBranchTip(const Cell3DPosition& pos) const;

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

    void printDebugInfo(const Cell3DPosition& pos) const;

    /** 
     * For a given position, return the branch index of the branch to which it belongs
     * @param pos input position 
     * @attention pos should not be tile root position or non-mesh position
     * @return branch index of the branch to which pos belongs, or -1 if invalid
     */
    short determineBranchForPosition(const Cell3DPosition& pos) const;
    
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

    /** 
     * Returns the position of the nearest tile root around pos
     * @param pos 
     * @return the position of the nearest tile root
     */
    const Cell3DPosition getNearestTileRootPosition(const Cell3DPosition& pos) const;
};

}
#endif /* MESH_RULE_MATCHER_HPP_ */
