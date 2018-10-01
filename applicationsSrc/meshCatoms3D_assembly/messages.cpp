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

#include "meshAssemblyBlockCode.hpp" //FIXME

static bool COLORIZE_PATH_SEARCH = false;
const Color visitedColor = ORANGE;

AbstractMeshSpanningTreeMessage*
DisassemblyTriggerMessage::buildNewMeshSpanningTreeMessage(BaseSimulator::BlockCode& bc,
                                                           const bool isAck) {
    MeshAssemblyBlockCode& mcbc = static_cast<MeshAssemblyBlockCode&>(bc);
    return new DisassemblyTriggerMessage(*mcbc.ruleMatcher, isAck);
}

void DisassemblyTriggerMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mcbc = *static_cast<MeshAssemblyBlockCode*>(bc);    
    
    // if (not --mcbc.numberExpectedAcksFromSubTree) {
    //     mcbc.catom->setColor(GREEN);
    //     const Cell3DPosition& parentPos =
    //         ruleMatcher.getTreeParentPosition(mcbc.catom->position);

    //     if (parentPos != mcbc.catom->position) {
    //         if (not mcbc.target->isInTarget(mcbc.catom->position)) {
    //             // mcbc.catom->setColor(WHITE);
    //             mcbc.catom->setVisible(false);
    //             // mcbc.world->deleteBlock(mcbc.catom);
    //         } else {
    //             mcbc.catom->setColor(mcbc.target->getTargetColor(mcbc.catom->position));
    //         }
            
    //         P2PNetworkInterface* parentItf = mcbc.catom->getInterface(parentPos);
    //         assert(parentItf->isConnected());
    //         acknowledgeToParent(mcbc, parentItf);
    //     } else {
    //         BaseSimulator::getWorld()->lattice->highlightCell(mcbc.catom->position, BLUE);
    //         cout << "-- Shape disassembly into CSG object done." << endl;
    //     }
    // }        
}

AbstractMeshSpanningTreeMessage* SubTreeScaffoldConstructionDoneMessage::
buildNewMeshSpanningTreeMessage(BaseSimulator::BlockCode& bc, const bool isAck) {
    return new SubTreeScaffoldConstructionDoneMessage(ruleMatcher, isAck);
}

void SubTreeScaffoldConstructionDoneMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mcbc = *static_cast<MeshAssemblyBlockCode*>(bc);    
    
    
    // if (not --mcbc.numberExpectedAcksFromSubTree) {        
    //     mcbc.catom->setColor(RED);

    //     const Cell3DPosition& parentPos =
    //         ruleMatcher.getTreeParentPosition(mcbc.catom->position);

    //     if (parentPos != mcbc.catom->position) {
    //         P2PNetworkInterface* parentItf = mcbc.catom->getInterface(parentPos);
    //         assert(parentItf->isConnected());
    //         acknowledgeToParent(mcbc, parentItf);
    //     } else {
    //         BaseSimulator::getWorld()->lattice->highlightCell(mcbc.catom->position, BLUE);
	// 		cout << "-- Scaffold construction done." << endl;
    //         mcbc.triggerMeshTraversalProcess();
    //     }
    // }
}

