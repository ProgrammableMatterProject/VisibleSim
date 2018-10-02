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

#include "messages.hpp"
#include "meshRuleMatcher.hpp"
#include "meshAssemblyBlockCode.hpp"

void RequestTargetCellMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);    
    
    // A neighboring catom requested an objective, check needs an make decision based on their distance.
    // FIXME: For now, route towards horizontal growth
    Cell3DPosition closestPos = Cell3DPosition(mabc.X_MAX, mabc.Y_MAX, mabc.Z_MAX);    
    int i; int cP_idx = -1;
    for (i = 0; i < N_BRANCHES; i++) {
        if (mabc.openPositions[i]
            and closestPos.dist_euclid(mabc.catom->position)
            >= mabc.catom->position.dist_euclid(*mabc.openPositions[i])) {

            closestPos = *mabc.openPositions[i];
            cP_idx = i;
        }
    }

    cout << cP_idx << endl;
    VS_ASSERT(cP_idx >= 0 and cP_idx < N_BRANCHES);

    // Update open position for that branch
    mabc.catomReqByBranch[cP_idx]--;
    if (mabc.catomReqByBranch[cP_idx] == 0) mabc.openPositions[cP_idx] = NULL;
    else *mabc.openPositions[cP_idx] = mabc.ruleMatcher->getBranchUnitOffset(cP_idx);

    // Send to requesting catom
    VS_ASSERT(destinationInterface->isConnected());
    mabc.sendMessage(new ProvideTargetCellMessage(closestPos),
                     destinationInterface, MSG_DELAY_MC, 0);
}

void ProvideTargetCellMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mcbc = *static_cast<MeshAssemblyBlockCode*>(bc);       
}
