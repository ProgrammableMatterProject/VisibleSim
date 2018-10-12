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
#include "catoms3DRotationsBlockCode.hpp"

void MoveNextModuleMessage::handle(BaseSimulator::BlockCode* bc) {
    Catoms3DRotationsBlockCode* mybc = static_cast<Catoms3DRotationsBlockCode*>(bc);

    if (nextBlockPos == mybc->catom->position) {
        // move
    } else {        
        if (mybc->catom->position[2] == nextBlockPos[2]) {
            const Cell3DPosition& nextHop = mybc->catom->position - Cell3DPosition(-1,0,0);
            P2PNetworkInterface* nextHopItf = mybc->catom->getInterface(nextHop);

            mybc->sendMessage(new MoveNextModuleMessage(nextBlockPos),
                              nextHopItf, 0, 0);
        } else {
            const Cell3DPosition& nextHop = mybc->catom->position - Cell3DPosition(0,0,-1);
            P2PNetworkInterface* nextHopItf = mybc->catom->getInterface(nextHop);

            mybc->sendMessage(new MoveNextModuleMessage(nextBlockPos),
                              nextHopItf, 0, 0);
        }
    }
}
