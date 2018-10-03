/**
 * @file   messages.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 14:13:13 2018
 * 
 * @brief  
 * 
 * 
 */

#include <iostream>
#include <sstream>

#include "utils.h"
#include "teleportationEvents.h"

#include "messages.hpp"
#include "meshRuleMatcher.hpp"
#include "meshAssemblyBlockCode.hpp"

void RequestTargetCellMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);    
    
    // A neighboring catom requested an objective, check needs an make decision based on their distance.
    // FIXME: For now, route towards horizontal growth
    const Cell3DPosition& nPos = sourceInterface->hostBlock->position;
    cout << "Spawnee Position: " << nPos << endl;
    Cell3DPosition closestPos = Cell3DPosition(mabc.X_MAX, mabc.Y_MAX, mabc.Z_MAX);    
    int i; int cP_idx = -1;
    for (i = 0; i < N_BRANCHES; i++) {
        if (mabc.openPositions[i])
            cout << "i: " << i << " - oP: " << *mabc.openPositions[i] << " - d: "
                 << mabc.lattice->getCellDistance(nPos, *mabc.openPositions[i]) << endl;
        
        if (mabc.openPositions[i]
            and mabc.fedCatomOnLastRound[i]
            and mabc.lattice->getCellDistance(nPos, closestPos)
            >= mabc.lattice->getCellDistance(nPos, *mabc.openPositions[i])) {

            closestPos = *mabc.openPositions[i];
            cP_idx = i;
        }
    }

    cout << cP_idx << endl;
    VS_ASSERT(cP_idx >= 0 and cP_idx < N_BRANCHES);

    // Update open position for that branch
    mabc.catomReqByBranch[cP_idx]--;
    if (mabc.catomReqByBranch[cP_idx] == 0) mabc.openPositions[cP_idx] = NULL;
    else *mabc.openPositions[cP_idx] += mabc.ruleMatcher->getBranchUnitOffset(cP_idx);

    // Send to requesting catom
    VS_ASSERT(destinationInterface->isConnected());
    mabc.sendMessage(new ProvideTargetCellMessage(closestPos),
                     destinationInterface, MSG_DELAY_MC, 0);
}

void ProvideTargetCellMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    mabc.targetPosition = tPos;

    Cell3DPosition nextHop;

    cout << "received tPos for module " << destinationInterface->hostBlock->blockId
         << ": " << tPos << endl;
    
    // Consider ack
    if (mabc.lattice->cellsAreAdjacent(mabc.catom->position, tPos)) {
        VS_ASSERT_MSG(mabc.lattice->isFree(tPos), "target branch position must be free!!!");
        nextHop = tPos;
    } else {
        // Determine position relative to branch that is being grown.
        /// If catom has been properly introduced, then it should be on one of the four bottom
        ///  connectors of the local coordinator. The simplest rule is to simply move along
        ///  the direction of the branch until reaching a neighboring position to the target;
        ///  this should always be possible.        
        const Cell3DPosition& relPos =
            mabc.catom->position - sourceInterface->hostBlock->position;

        cout << "Relative position to local coordinator: " <<  relPos << endl;
        
        // Deduce next position
        nextHop = mabc.catom->position + mabc.ruleMatcher->
            getBranchUnitOffset(mabc.ruleMatcher->
                                getBranchIndexForNonRootPosition(mabc.normalize_pos(tPos)));
    }
    
    mabc.scheduler->schedule(
        new TeleportationStartEvent(getScheduler()->now(), mabc.catom, nextHop));
    // awaitKeyPressed();
}
