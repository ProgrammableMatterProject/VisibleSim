#include "coatingBlockCode.hpp"

using namespace Catoms3D;

CoatingBlockCode::CoatingBlockCode(Catoms3DBlock *host) : Catoms3DBlockCode(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;

    // Registers a callback (handleSampleMessage) to the message of type SAMPLE_MSG_ID
    // addMessageEventFunc2(SAMPLE_MSG_ID,
    //                      std::bind(&CoatingBlockCode::handleSampleMessage, this,
    //                                std::placeholders::_1, std::placeholders::_2));

    // set the module pointer
    module = static_cast<Catoms3DBlock*>(hostBlock);
  }

void CoatingBlockCode::startup() {
    console << "start";

    // Sample distance coloring algorithm below
    if (module->blockId==1) { // Master ID is 1
        module->setColor(RED);
        distance = 0;
        sendMessageToAllNeighbors("Sample Broadcast",
                                  new MessageOf<int>(SAMPLE_MSG_ID,distance),100,200,0);
    } else {
        distance = -1; // Unknown distance
        hostBlock->setColor(LIGHTGREY);
    }

    // Additional initialization and algorithm start below
    // ...
}

void CoatingBlockCode::handleSampleMessage(MessagePtr msgPtr, P2PNetworkInterface* sender) {
    MessageOf<int>* msg = static_cast<MessageOf<int>*>(msgPtr.get());

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

void CoatingBlockCode::onMotionEnd() {
    console << " has reached its destination" << "\n";

    // do stuff
    // ...
}

void CoatingBlockCode::processLocalEvent(EventPtr pev) {
    MessagePtr message;
    stringstream info;

    // Do not remove line below
    BlockCode::processLocalEvent(pev);

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

void CoatingBlockCode::onBlockSelected() {
    // Debug stuff:
    cerr << endl << "--- PRINT MODULE " << *module << "---" << endl;
}

void CoatingBlockCode::onAssertTriggered() {
    console << " has triggered an assert" << "\n";

    // Print debugging some info if needed below
    // ...
}

// bool CoatingBlockCode::parseUserCommandLineArgument(int &argc, char **argv[]) {
//     /* Reading the command line */
//     if ((argc > 0) && ((*argv)[0][0] == '-')) {
//         switch((*argv)[0][1]) {

//             // Single character example: -b
//             case 'b':   {
//                 cout << "-b option provided" << endl;
//                 return true;
//             } break;

//             // Composite argument example: --foo 13
//             case '-': {
//                 string varg = string((*argv)[0] + 2); // argv[0] without "--"
//                 if (varg == string("foo")) { //
//                     try {
//                         int fooArg = stoi((*argv)[1]);
//                         argc--;
//                         (*argv)++;
//                     } catch(std::logic_error&) {}

//                     cout << "--foo option provided with value: " << fooArg << endl;
//                 } else return false;

//                 return true;
//             }

//             default: cerr << "Unrecognized command line argument: " << (*argv)[0] << endl;
//         }
//     }

//     return false;
// }
