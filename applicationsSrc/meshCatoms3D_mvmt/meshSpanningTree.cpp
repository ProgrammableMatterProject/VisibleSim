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

#include "meshCatoms3DBlockCode_mvmt.hpp" //FIXME:

using namespace MeshSpanningTree;

bool MeshSpanningTreeRuleMatcher::isOnPartialBorderMesh(const Cell3DPosition& pos) const {
    const int zCoeff = pos[2] / B;
    const int intB = B;
    
    return (pos[2] % B == 0 and (IS_ODD(zCoeff) // Planar case (lower bounds)
                                 and ((pos[0] < -intB * (zCoeff / 2)
                                       or pos[1] < -intB * (zCoeff / 2))) ))
        or (pos[2] % B != 0 and ((pos[0] > 0 // Downward oblique case (upper bounds)
                                  and pos[0] > (int)(B * ((X_MAX - (zCoeff / 2) * B) / B)))
                                 or (pos[1] > 0 and
                                     pos[1] > (int)(B * ((Y_MAX - (zCoeff / 2) * B) / B))))
            );
}

bool MeshSpanningTreeRuleMatcher::isTileRoot(const Cell3DPosition& pos) const {
    return (int)(abs(pos[0])) % B == 0 and (int)(abs(pos[1])) % B == 0
        and (int)(abs(pos[2])) % B == 0;
}

bool MeshSpanningTreeRuleMatcher::upwardBranchRulesApply(const Cell3DPosition& own,
                                                         const Cell3DPosition& other) const {
    const int zCoeff = other[2] / B;
    
    // Module is on branch if z is NOT a multiple of B
    return not isTileRoot(own) and own[2] % B != 0
        and (
            // In that case, only allow upward transmission
            own[2] < other[2]
            // Unless neighbor is not on branch
            and (other[2] % B != 0
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
    return own[2] % B == 0 and own[2] == other[2]
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

bool MeshSpanningTreeMessage::forwardToNeighbors(MeshCatoms3DBlockCode& mcbc) {
    return false;
}

void MeshSpanningTreeMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshCatoms3DBlockCode& mcbc = *static_cast<MeshCatoms3DBlockCode*>(bc);    
    
    if (!mcbc.stParent) {
        mcbc.stParent = destinationInterface;
        mcbc.catom->setColor(BLUE);
    } else {
        cout << mcbc.catom->blockId << " " << mcbc.catom->position << endl;
        mcbc.catom->setColor(WHITE);
        awaitKeyPressed();
        assert(!mcbc.stParent);
    }

    for (const Cell3DPosition& pos :
             mcbc.lattice->getActiveNeighborCells(mcbc.catom->position)) {
                
        P2PNetworkInterface* itf = mcbc.catom->getInterface(pos);
        assert (itf != NULL);


        const Cell3DPosition& myPos = mcbc.catom->position;
        if (mcbc.moduleInSpanningTree(pos)
            and (
                 (
                  not ruleMatcher.isOnPartialBorderMesh(myPos)
                  and (
                       ruleMatcher.planarBranchRulesApply(myPos, pos)
                       or ruleMatcher.meshRootBranchRulesApply(myPos, pos)
                       or ruleMatcher.upwardBranchRulesApply(myPos, pos)
                       )
                  ) 
                 or ruleMatcher.partialBorderMeshRulesApply(myPos, pos)
                 )
            and itf != mcbc.stParent) {
            mcbc.sendMessage("Spanning Tree A",
                             new MeshSpanningTreeMessage(ruleMatcher),
                             itf, MSG_DELAY_MC, 0);
            mcbc.expectedConfirms++;
        }
    }

    if (not mcbc.expectedConfirms)
        mcbc.sendMessage("Spanning Tree B",
                    new MeshSpanningTreeAckMessage(),
                    mcbc.stParent, MSG_DELAY_MC, 0);        
}

bool MeshSpanningTreeAckMessage::acknowledgeToParent() {
    return false;
}

void MeshSpanningTreeAckMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshCatoms3DBlockCode& mcbc = *static_cast<MeshCatoms3DBlockCode*>(bc);

    if (not --mcbc.expectedConfirms) {
        mcbc.catom->setColor(RED);

        if (mcbc.stParent) {
            mcbc.sendMessage("Spanning Tree B",
                    new MeshSpanningTreeAckMessage(),
                        mcbc.stParent, MSG_DELAY_MC, 0);
        }
    }
}
