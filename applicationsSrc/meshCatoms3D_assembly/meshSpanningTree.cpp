/**
 * @file   meshSpanningTree.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Fri Jul 13 13:27:03 2018
 * 
 * @brief  
 * 
 * 
 */

#include "meshSpanningTree.hpp"

#include "network.h"
#include "utils.h"
#include "catoms3DWorld.h"

using namespace MeshSpanningTree;
using namespace BaseSimulator;
using namespace utils;
using namespace Catoms3D;

void MeshSpanningTreeRuleMatcher::printDebugInfo(const Cell3DPosition& pos) const {
  cout << "--- DEBUG INFO: " << pos << endl;
  cout << "getTreeParentPosition(pos): " << getTreeParentPosition(pos) << endl;
  cout << "isTileRoot(pos): " << isTileRoot(pos) << endl;
  cout << "isOnPartialBorderMesh(pos): " << isOnPartialBorderMesh(pos) << endl;
  cout << "isOnXBranch(pos): " << isOnXBranch(pos) << endl;
  cout << "isOnXBorder(pos): " << isOnXBorder(pos) << endl;
  cout << "isOnYBranch(pos): " << isOnYBranch(pos) << endl;
  cout << "isOnYBorder(pos): " << isOnYBorder(pos) << endl;
  cout << "isOnZBranch(pos): " << isOnZBranch(pos) << endl;
  cout << "isOnRevZBranch(pos): " << isOnRevZBranch(pos) << endl;
  cout << "isOnMinus45DegZBranch(pos): " << isOnMinus45DegZBranch(pos) << endl;
  cout << "isOnPlus45DegZBranch(pos): " << isOnPlus45DegZBranch(pos) << endl;
  cout << "--- END DEBUG INFO ---" << endl << endl;
}

bool MeshSpanningTreeRuleMatcher::isInMesh(const Cell3DPosition& pos) const {
    return isOnXBranch(pos) or isOnYBranch(pos) or isOnZBranch(pos)
        or isOnRevZBranch(pos) or isOnMinus45DegZBranch(pos) or isOnPlus45DegZBranch(pos);
}

bool MeshSpanningTreeRuleMatcher::isOnXBranch(const Cell3DPosition& pos) const {
    return m_mod(pos[1], B) == 0 and m_mod(pos[2], B) == 0;
}

bool MeshSpanningTreeRuleMatcher::isOnXBorder(const Cell3DPosition& pos) const {
    return pos[1] == (int)(-(int)(pos[2] / B) / 2 * B) and m_mod(pos[2], B) == 0;
}


bool MeshSpanningTreeRuleMatcher::isOnYBranch(const Cell3DPosition& pos) const {
    return m_mod(pos[0], B) == 0 and m_mod(pos[2], B) == 0;
}

bool MeshSpanningTreeRuleMatcher::isOnYBorder(const Cell3DPosition& pos) const {
    return pos[0] == (int)(-(int)(pos[2] / B) / 2 * B) and m_mod(pos[2], B) == 0;
}

bool MeshSpanningTreeRuleMatcher::isOnZBranch(const Cell3DPosition& pos) const {
    const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
    const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
    
    return m_mod(x, B) == 0 and m_mod(y, B) == 0;
}

bool MeshSpanningTreeRuleMatcher::isOnRevZBranch(const Cell3DPosition& pos) const {
    const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
    const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
    const int z = pos[2];
    
    return m_mod(x, B) == m_mod(y, B) and m_mod(z, B) == m_mod(B - x, B);
}

bool MeshSpanningTreeRuleMatcher::isOnMinus45DegZBranch(const Cell3DPosition& pos) const {
    const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
    const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
    const int z = pos[2];
    
    return (m_mod(x, B) + m_mod(z, B) == (int)B and m_mod(y, B) == 0);
}

bool MeshSpanningTreeRuleMatcher::isOnPlus45DegZBranch(const Cell3DPosition& pos) const {
    const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
    const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
    const int z = pos[2];

    return (m_mod(y, B) + m_mod(z, B) == (int)B and m_mod(x, B) == 0);
}

bool MeshSpanningTreeRuleMatcher::isOnPartialBorderMesh(const Cell3DPosition& pos) const {
    return (m_mod(pos[2], B) == 0 and not isTileRoot(pos)
            and (
                (isOnXBranch(pos)
                 and (pos[0] - m_mod(pos[0], B)  < - (pos[2] / 2)) )
                or (isOnYBranch(pos)
                     and (pos[1] - m_mod(pos[1], B) < - (pos[2] / 2)) )
                ))
        or (m_mod(pos[2], B) != 0 and // Downward oblique case (ub)
            // (pos[2] - pos[2] % (int)B) is expected z of tileRoot
            ( (m_mod(pos[0], B) > 0         
               and pos[0] + (int)B - m_mod(pos[0], B) >
               (int)X_MAX - (pos[2] - m_mod(pos[2], B)) / 2 )
              or (m_mod(pos[1], B) > 0 
                  and pos[1] + (int)B - m_mod(pos[1], (int)B) >
                  (int)Y_MAX - (pos[2] - m_mod(pos[2], (int)B)) / 2 )
                )
            );
}

bool MeshSpanningTreeRuleMatcher::isTileRoot(const Cell3DPosition& pos) const {    
    return m_mod(pos[0], B) == 0 and m_mod(pos[1], B) == 0 and m_mod(pos[2], B) == 0;
}

bool MeshSpanningTreeRuleMatcher::upwardBranchRulesApply(const Cell3DPosition& own,
                                                         const Cell3DPosition& other) const {
    const int zCoeff = other[2] / B;
    
    // Module is on branch if z is NOT a multiple of B
    return not m_mod(own[2], B) == 0
        and (
            // In that case, only allow upward transmission
            own[2] < other[2]
            // Unless neighbor is not on branch
            and (m_mod(other[2], B) != 0
                 // Transmitting to the next root only along z axis from (0,0,0)
                 or (isTileRoot(other)
                     and (
                         other[0] == (int)(-zCoeff / 2 * B)
                         and other[0] == other[1]
                         and (
                             (IS_EVEN(zCoeff)
                              // If o.z / B even, then we need to go up/backward
                              and other - own == Cell3DPosition(-1, -1, 1))
                             or
                             (IS_ODD(zCoeff)
                              // if o.z / B odd, then we need to go up/forward
                              and other - own == Cell3DPosition(0, 0, 1))
                             )
                         )
                     )
                )
            );            
}

bool MeshSpanningTreeRuleMatcher::planarBranchRulesApply(const Cell3DPosition& own,
                                                         const Cell3DPosition& other) const {
    // Module is on plan if z is a multiple of B
    return m_mod(own[2], B) == 0 and own[2] == other[2]
        and (
            // In that case, only allow transmission to increasing x...
            // (except if other is a root as transmission to roots occur along y axis,
            //   and we are not on lower border)
            (own[0] < other[0] and
             (not isTileRoot(other) or (own[1] + own[2] / 2) < (int)B))
            // ... or increasing y.
            or own[1] < other[1]
            );
}

bool MeshSpanningTreeRuleMatcher::meshRootBranchRulesApply(const Cell3DPosition& own,
                                                           const Cell3DPosition& other) const {
    // Mesh root is responsible for upward propagation
    return isTileRoot(own)
        and (
            // In that case, only allow transmission to increasing z, in all directions
             own[2] < other[2]
             );
}

bool MeshSpanningTreeRuleMatcher::partialBorderMeshRulesApply(const Cell3DPosition& own,
                                                              const Cell3DPosition& other) const {
    // Gotta decide that we are on a mesh with no root using xmax, ymax, xmax;    
    return isOnPartialBorderMesh(other)
        and (
            // Mesh root initiates on a border initiates normally forbidden transmission
            isTileRoot(own)
            or
            (isOnPartialBorderMesh(own)
             and ((own[2] % B == 0 and own[2] == other[2] and (own[0] > other[0]
                                                               or own[1] > other[1]))
                  or (own[2] % B != 0 and other[2] < own[2]))
                ));
}

bool MeshSpanningTreeRuleMatcher::shouldSendToNeighbor(const Cell3DPosition& own,
                                                       const Cell3DPosition& other) const {
    return (not isOnPartialBorderMesh(own)
            and (
                planarBranchRulesApply(own, other)
                or meshRootBranchRulesApply(own, other)
                or upwardBranchRulesApply(own, other)
                )
        ) 
        or partialBorderMeshRulesApply(own, other);
}

const Cell3DPosition
MeshSpanningTreeRuleMatcher::getTreeParentPosition(const Cell3DPosition& pos) const {
    if (isTileRoot(pos)) {
        const int zCoeff = pos[2] / B;
        // Mesh Root
        if (pos[0] == 0 and pos[1] == 0 and pos[2] == 0) return pos;
        // Upward propagating branch, parent is branch predecessor
        else if (pos[0] == (int)(-zCoeff / 2 * B) and pos[1] == pos[0]) {
            return IS_EVEN(zCoeff) ?
                pos + Cell3DPosition(1, 1, -1) : pos + Cell3DPosition(0, 0, -1);
        }
    }

    if (isOnPartialBorderMesh(pos)) {
        // parent is either x - 1 or y - 1 if planar
        if (isOnYBranch(pos)) return pos + Cell3DPosition(0,1,0);
        else if (isOnXBranch(pos)) return pos + Cell3DPosition(1,0,0);
        //  or z + 1 if on an incomplete downward branch
        else if (isOnZBranch(pos)) return pos + Cell3DPosition(0,0,1);
        else if (isOnRevZBranch(pos)) return pos + Cell3DPosition(-1,-1,1);
        else if (isOnMinus45DegZBranch(pos)) return pos + Cell3DPosition(-1,0,1);
        else if (isOnPlus45DegZBranch(pos)) return pos + Cell3DPosition(0,-1,1);
        
        assert(false);
    }
    
    // Planar cases X and Y
    else if (isOnYBranch(pos) and !isOnXBorder(pos)) return pos + Cell3DPosition(0,-1,0);
    else if (isOnXBranch(pos) and !isOnYBorder(pos)) return pos + Cell3DPosition(-1,0,0);
    // Branch cases
    else if (isOnZBranch(pos)) return pos + Cell3DPosition(0,0,-1);
    else if (isOnRevZBranch(pos)) return pos + Cell3DPosition(1,1,-1);
    else if (isOnMinus45DegZBranch(pos)) return pos + Cell3DPosition(1,0,-1);
    else if (isOnPlus45DegZBranch(pos)) return pos + Cell3DPosition(0,1,-1);

    assert(false);
}

unsigned int MeshSpanningTreeRuleMatcher::
getNumberOfExpectedSubTreeConfirms(const Cell3DPosition& pos) const {
    unsigned int expectedConfirms = 0;
    for (const Cell3DPosition& nPos :
             Catoms3D::getWorld()->lattice->getActiveNeighborCells(pos)) {
        if (shouldSendToNeighbor(pos, nPos)) expectedConfirms++;
    }

    OUTPUT << "NumberExpectedConfirms for " << pos << " : " << expectedConfirms << endl;
    
    return expectedConfirms;
}

int AbstractMeshSpanningTreeMessage::forwardToNeighbors(BaseSimulator::BlockCode& bc,
                                                        const P2PNetworkInterface* except_itf) {
    Catoms3DBlock *catom = static_cast<Catoms3DBlock*>(bc.hostBlock);
    
    uint sentCounter = 0;    
    for (const Cell3DPosition& pos :
             bc.lattice->getActiveNeighborCells(catom->position)) {
                
        P2PNetworkInterface* itf = catom->getInterface(pos);
        assert (itf != NULL);        

        if (itf != except_itf and ruleMatcher.shouldSendToNeighbor(catom->position, pos)) {
            AbstractMeshSpanningTreeMessage *message =
                buildNewMeshSpanningTreeMessage(bc, false);

            assert(message);
            
            bc.sendMessage(message, itf, MSG_DELAY_MC, 0);
            sentCounter++;
        }
    }

    // TODO: Should be stored in some kind of counter dictionary with a different ID foreach
    //  messaging session (the could be selected by the first sender if it propagates down
    //  a subtree or using a constant id if propagated up from several leaves.
    return sentCounter;
}

bool AbstractMeshSpanningTreeMessage::acknowledgeToParent(BaseSimulator::BlockCode& bc,
                                                          P2PNetworkInterface* parent_itf) {
    if (parent_itf) {
        AbstractMeshSpanningTreeMessage *ack =
            buildNewMeshSpanningTreeMessage(bc, true);

        assert(ack);
        
        bc.sendMessage(ack, parent_itf, MSG_DELAY_MC, 0);

        return true;
    }

    return false;
}
