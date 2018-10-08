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
    const Cell3DPosition& rPos = nPos - mabc.catom->position;
    short epd = mabc.getEntryPointDirectionForCell(nPos);
    const Cell3DPosition& tPos =
        mabc.targetForEntryPoint[mabc.getEntryPointDirectionForCell(nPos)];
    cout << "Spawnee Position: " << nPos 
         << " -- Target Position: " << tPos
         << " -- Relative Position: " << rPos
         << " -- epd" << epd << endl;
    
    // Send to requesting catom
    VS_ASSERT(destinationInterface->isConnected());
    mabc.sendMessage(new ProvideTargetCellMessage(tPos),
                     destinationInterface, MSG_DELAY_MC, 0);
}

void ProvideTargetCellMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    mabc.coordinatorPos = sourceInterface->hostBlock->position;
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
            mabc.catom->position - mabc.coordinatorPos;

        cout << "Relative position to local coordinator: " <<  relPos << endl;
        
        // Deduce next position
        BranchIndex bi = mabc.ruleMatcher->
            getBranchIndexForNonRootPosition(mabc.norm(tPos));

        if (bi > 3)
            nextHop = mabc.catom->position + mabc.ruleMatcher->getBranchUnitOffset(bi);
        else if (bi == ZBranch) {
            if (mabc.coordinatorPos == mabc.meshSeedPosition)
                nextHop = mabc.catom->position + Cell3DPosition(-1,-1,2);
            else
                nextHop = mabc.catom->position + Cell3DPosition(-1,-1,1);
        } else if (bi == RevZBranch) {
            nextHop = mabc.catom->position + Cell3DPosition(0,0,1);
        } else {
            throw NotImplementedException();
        }
    }
    
    mabc.scheduler->schedule(
        new TeleportationStartEvent(getScheduler()->now(), mabc.catom, nextHop));

#ifdef INTERACTIVE_MODE
    awaitKeyPressed();
#endif
}
