#include "dodderBlockCode.hpp"

using namespace Catoms3D;

WrappingDirection DodderBlockCode::currentDirection = WrappingDirection::CCW;

DodderBlockCode::DodderBlockCode(Catoms3DBlock *host) : Catoms3DBlockCode(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;

    // Registers a callback (handleSampleMessage) to the message of type SAMPLE_MSG_ID
    addMessageEventFunc2(SAMPLE_MSG_ID,
                         std::bind(&DodderBlockCode::handleSampleMessage, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Set the module pointer
    module = static_cast<Catoms3DBlock*>(hostBlock);
  }


  void DodderBlockCode::startup() {
      console << "start";
      if (isSupport) {
            state = SUPPORT;
            console << " (Support Block)\n";
            module->setColor(GREEN);
            pair<Cell3DPosition, uint8_t> nextTipPos = getNextTipPosition(module->position, 5);
            //BaseSimulator::getWorld()->addBlock(0, buildNewBlockCode, nextTipPos.first, RED, 0);
            auto neighState = module->getLocalNeighborhoodState();
            console << "neighState: " << neighState << "\n";
            while (nextTipPos.first != Cell3DPosition()) {
                console << " next tip position: " << nextTipPos.first << "\n";
                if(lattice->cellHasBlock(nextTipPos.first)) {
                    break; // Stop if the next position already has a block
                }
                BaseSimulator::getWorld()->addBlock(0, buildNewBlockCode, nextTipPos.first, RED, 0);
                auto newBlock = dynamic_cast<DodderBlockCode*>(lattice->getBlock(nextTipPos.first)->blockCode);
                newBlock->state = SUPPORT; // Set the new block state to SUPPORT
                nextTipPos = getNextTipPosition(nextTipPos.first, nextTipPos.second);
                neighState = newBlock->module->getLocalNeighborhoodState();
                console << "neighState: " << neighState << "\n";
            }

            


            // while (nextTipPos != Cell3DPosition()) {
            //     console << " next tip position: " << nextTipPos << "\n";
            //     BaseSimulator::getWorld()->addBlock(0, buildNewBlockCode, nextTipPos, RED, 0);
            //     auto newBlock = dynamic_cast<DodderBlockCode*>(lattice->getBlock(nextTipPos)->blockCode);
            //     newBlock->state = SUPPORT; // Set the new block state to SUPPORT
            //     nextTipPos = getNextTipPosition(nextTipPos, prevDir);
            //     neighState = newBlock->module->getLocalNeighborhoodState();
            //     console << "neighState: " << neighState << "\n";
            //     // if (nextTipPos != Cell3DPosition()) {
            //     //     console << " next tip position: " << nextTipPos << "\n";
            //     //     BaseSimulator::getWorld()->addBlock(0, buildNewBlockCode, nextTipPos, RED, 0);
            //     // } else {
            //     //     console << " no further tip position found\n"; 
            //     // }    
            // }
      }

}

bool DodderBlockCode::isPositionConnectedToObject(const Cell3DPosition &pos) { 
    for(auto &activeNeighborsPos: lattice->getActiveNeighborCells(pos)) {
        auto neighborBlock = dynamic_cast<DodderBlockCode*>(lattice->getBlock(activeNeighborsPos)->blockCode);
        if(neighborBlock && neighborBlock->state == OBJECT) {
            return true; // Found a connected object
        }
    }
    return false; // No connected object found
 }

Cell3DPosition DodderBlockCode::getRelativePosition(const Cell3DPosition &currentPos, const Cell3DPosition &nextPos) {
    // Get the next position in the current direction
    Cell3DPosition relativePos = nextPos - currentPos;
    return relativePos;
}

pair<Cell3DPosition, uint8_t> DodderBlockCode::getNextTipPosition(Cell3DPosition &currentTipPosition, uint8_t dir) {
    vector<Cell3DPosition> neighbors = lattice->getFreeNeighborCells(currentTipPosition);
    if (neighbors.empty()) {
        console << "No free neighbors found for the dodder block at position " << currentTipPosition << "\n";
        return {Cell3DPosition(), 0}; // Return an empty position if no neighbors are available
    }

    Cell3DPosition nextTipPosition = lattice->getCellInDirection(currentTipPosition, dir);
    console << "Next tip position in direction " << dir << ": " << nextTipPosition << "\n";
    if(isPositionConnectedToObject(nextTipPosition)) {
        console << "Next tip position " << nextTipPosition << " is connected to an object.\n";
        return {nextTipPosition, dir}; // Return the next position if it is connected to an object
    } else {
        if (currentDirection == WrappingDirection::CCW) {
            // Try directions in priority order for CCW
            const std::vector<uint8_t> ccwDirs = {2, 3, 4, 5, 11, 8};
            for (uint8_t nextDir : ccwDirs) {
                nextTipPosition = lattice->getCellInDirection(currentTipPosition, nextDir);
                if (isPositionConnectedToObject(nextTipPosition)) {
                    console << "Next tip position " << nextTipPosition << " is connected to an object in CCW direction " << int(nextDir) << ".\n";
                    return {nextTipPosition, nextDir};
                }
            }
            console << "No connected object found in CCW direction from " << currentTipPosition << "\n";
            return {currentTipPosition, dir};
        }
    }

    // get neighbor to go up while connected to an object
    // for (const auto &neighbor : neighbors) {
    //     Cell3DPosition relativePos = getRelativePosition(currentTipPosition, neighbor);
    //     // Prefer neighbors above current position and connected to object, with CCW wrapping direction and x >= current x
    //     if (neighbor.pt[2] > currentTipPosition.pt[2]
    //         && isPositionConnectedToObject(neighbor)
    //         && currentDirection == WrappingDirection::CCW
    //         && neighbor.pt[0] >= currentTipPosition.pt[0]
    //         // && neighbor.pt[1] >= currentTipPosition.pt[1] 
    //     ) {    
    //         return neighbor; // Return the first valid neighbor found
    //     }
    // }
    // If no valid neighbor is found, return an empty position
    return {Cell3DPosition(), dir};
}

void DodderBlockCode::handleSampleMessage(std::shared_ptr<Message> _msg,
                                               P2PNetworkInterface* sender) {
    MessageOf<int>* msg = static_cast<MessageOf<int>*>(_msg.get());

    int d = *msg->getData() + 1;
    console << " received d =" << d << " from " << sender->getConnectedBlockId() << "\n";

    if (distance == -1 || distance > d) {
        console << " updated distance = " << d << "\n";
        distance = d;
        module->setColor(Colors[distance % NB_COLORS]);

        // Broadcast to all neighbors but ignore sender
        sendMessageToAllNeighbors("Sample Broadcast",
                                  new MessageOf<int>(SAMPLE_MSG_ID,distance),100,200,1,sender);
    }
}

void DodderBlockCode::onMotionEnd() {
    console << " has reached its destination" << "\n";

    // do stuff
    // ...
}

void DodderBlockCode::processLocalEvent(EventPtr pev) {
    std::shared_ptr<Message> message;
    stringstream info;

    // Do not remove line below
    Catoms3DBlockCode::processLocalEvent(pev);

    switch (pev->eventType) {
        case EVENT_ADD_NEIGHBOR: {
            // Do something when a neighbor is added to an interface of the module
            break;
        }

        case EVENT_REMOVE_NEIGHBOR: {
            // Do something when a neighbor is removed from an interface of the module
            break;
        }
    }
}

/// ADVANCED BLOCKCODE FUNCTIONS BELOW

void DodderBlockCode::onBlockSelected() {
    // Debug stuff:
    cerr << endl << "--- PRINT MODULE " << *module << "---" << endl;
}

void DodderBlockCode::onAssertTriggered() {
    console << " has triggered an assert" << "\n";

    // Print debugging some info if needed below
    // ...
}

bool DodderBlockCode::parseUserCommandLineArgument(int &argc, char **argv[]) {
    /* Reading the command line */
    if ((argc > 0) && ((*argv)[0][0] == '-')) {
        switch((*argv)[0][1]) {

            // Single character example: -b
            case 'b':   {
                cout << "-b option provided" << endl;
                return true;
            } break;

            // Composite argument example: --foo 13
            case '-': {
                string varg = string((*argv)[0] + 2); // argv[0] without "--"
                if (varg == string("foo")) { //
                    int fooArg;
                    try {
                        fooArg = stoi((*argv)[1]);
                        argc--;
                        (*argv)++;
                    } catch(std::logic_error&) {
                        stringstream err;
                        err << "foo must be an integer. Found foo = " << argv[1] << endl;
                        throw CLIParsingError(err.str());
                    }

                    cout << "--foo option provided with value: " << fooArg << endl;
                } else return false;

                return true;
            }

            default: cerr << "Unrecognized command line argument: " << (*argv)[0] << endl;
        }
    }

    return false;
}

void DodderBlockCode::parseUserBlockElements(TiXmlElement *config) {
    const char *attr = config->Attribute("support");
    isSupport = (attr?Simulator::extractBoolFromString(attr):false);
    if (isSupport) {
        std::cout << getId() << " is Support!" << std::endl; // complete with your code
    }
}

string DodderBlockCode::onInterfaceDraw() {
    stringstream trace;
    trace << "Some value " << 123;
    return trace.str();
}
