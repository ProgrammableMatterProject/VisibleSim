#include "energyRegulatorBBBlockCode.hpp"

using namespace BlinkyBlocks;

EnergyRegulatorBBBlockCode::EnergyRegulatorBBBlockCode(BlinkyBlocksBlock *host)
    : BlinkyBlocksBlockCode(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;

    // Registers a callback (handleSampleMessage) to the message of type SAMPLE_MSG_ID
    addMessageEventFunc2(BROADCAST_MSG_ID,
                         std::bind(&EnergyRegulatorBBBlockCode::handleBroadcastMessage, this,
                                   std::placeholders::_1, std::placeholders::_2));
    addMessageEventFunc2(CONVERGECAST_MSG_ID,
                         std::bind(&EnergyRegulatorBBBlockCode::handleConvergecastMessage, this,
                                   std::placeholders::_1, std::placeholders::_2));
    addMessageEventFunc2(INITIALENERGY_MSG_ID,
                         std::bind(&EnergyRegulatorBBBlockCode::handleInitialEnergyMessage, this,
                                   std::placeholders::_1, std::placeholders::_2));
    addMessageEventFunc2(UPDATEENERGY_MSG_ID,
                         std::bind(&EnergyRegulatorBBBlockCode::handleUpdateEnergyMessage, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Set the module pointer
    module = static_cast<BlinkyBlocksBlock *>(hostBlock);
}

float EnergyRegulatorBBBlockCode::totalEnergy = 0.0f;
vector<float> EnergyRegulatorBBBlockCode::availableEnergy = {2000.0f};

void EnergyRegulatorBBBlockCode::startup() {
    console << "start";

    if (isFeedingBlock) {
        updateColor(10, 0, 0);
        nbWaitedAnswers = sendMessageToAllNeighbors("Sample Broadcast",
                                                    new Message(BROADCAST_MSG_ID), 100, 200, 0);
    } else {
        totalEnergy += meanEnergy100;
        availableEnergy[0] -= meanEnergy100;
        updateColor(100, 100, 100);
        myEnergy = getCurrentConsumption();
        I_total = 0.0f;
    }

    // // Sample distance coloring algorithm below
    // if (module->blockId == 1) { // Master ID is 1
    //     module->setColor(RED);
    //     distance = 0;
    //     sendMessageToAllNeighbors("Sample Broadcast",
    //                               new MessageOf<int>(SAMPLE_MSG_ID,distance),100,200,0);
    // } else {
    //     distance = -1; // Unknown distance
    //     hostBlock->setColor(LIGHTGREY);
    // }

    // Additional initialization and algorithm start below
    // ...
}

float EnergyRegulatorBBBlockCode::getCurrentConsumption() {
    // Get the current consumption of the module
    float RGB = r + g + b;
    float currentConsumption = 0.617f * RGB + 10.33f;
    return currentConsumption;
}

void EnergyRegulatorBBBlockCode::updateColor(int _r, int _g, int _b) {
    // Update the color of the module
    if (_r > 100 || _g > 100 || _b > 100 || _r < 0 || _g < 0 || _b < 0) {
        console << "Color values should be between 0 and 100" << "\n";
        return;
    }
    r = _r;
    g = _g;
    b = _b;
    module->setColor(Color(_r * 2.55f, _g * 2.55f, _b * 2.55f));
}

void EnergyRegulatorBBBlockCode::handleBroadcastMessage(std::shared_ptr<Message> _msg,
                                                        P2PNetworkInterface *sender) {
    // MessageOf<pair<int, float>> *msg = dynamic_cast<MessageOf<pair<int, float>> *>(_msg.get());
    // int recvRound = msg->getData()->first;
    // float recvEnergy = msg->getData()->second;
    if (!parent) {
        parent = sender;
        nbWaitedAnswers = sendMessageToAllNeighbors(
            "Sample Broadcast", new Message(BROADCAST_MSG_ID), 100, 200, 1, sender);
        if (nbWaitedAnswers == 0) {
            console << "No neighbors to send the message to!\n";
            sendMessage("Convergecast energy", new MessageOf<float>(CONVERGECAST_MSG_ID, myEnergy),
                        parent, 100, 200);
        }
    } else {
        sendMessage("Convergecast energy", new MessageOf<float>(CONVERGECAST_MSG_ID, -1), sender,
                    100, 200);
    }
}

void EnergyRegulatorBBBlockCode::handleConvergecastMessage(std::shared_ptr<Message> _msg,
                                                           P2PNetworkInterface *sender) {
    MessageOf<float> *msg = dynamic_cast<MessageOf<float> *>(_msg.get());
    console << "Received convergecast message from " << sender->getConnectedBlockId() << "\n";
    float recvEnergy = *msg->getData();
    if (recvEnergy != -1) {
        I_total += recvEnergy;
    }
    nbWaitedAnswers--;
    children.push_back(sender);
    if (nbWaitedAnswers == 0) {
        I_total += myEnergy;
        if (!parent) {
            cout << "total energy: " << I_total << endl;
            for (auto child : children) {
                sendMessage("set initial energy",
                            new MessageOf<float>(INITIALENERGY_MSG_ID, I_total), child, 100, 200);
            }
        } else {
            sendMessage("Convergecast energy", new MessageOf<float>(CONVERGECAST_MSG_ID, I_total),
                        parent, 100, 200);
        }
    }
}

void EnergyRegulatorBBBlockCode::handleInitialEnergyMessage(std::shared_ptr<Message> _msg,
                                                            P2PNetworkInterface *sender) {
    I_total = *dynamic_cast<MessageOf<float> *>(_msg.get())->getData();
    console << "Received initial energy message from " << sender->getConnectedBlockId() << " "
            << I_total << "\n";
    if (!parent) {
        parent = sender;
        auto *senderBlock = dynamic_cast<EnergyRegulatorBBBlockCode *>(BaseSimulator::getWorld()->getBlockById(sender->getConnectedBlockId())->blockCode);
        updateColor(senderBlock->r , senderBlock->g , senderBlock->b);
        myEnergy = getCurrentConsumption();
        if (2000 - (I_total + myEnergy) < 229) {
            updateColor(r - r * 0.1026, g - g*0.1026, b - b*0.1026);
            if (r <= 10 or g <= 10 or b <= 10) {
                console << r << " " << g << " " << b << "\n";
                console << "Color values should be between 0 and 100" << "\n";
                updateColor(100, 0, 0);
                return;
            }
            myEnergy = getCurrentConsumption();
            I_total += myEnergy;
            console << "adapted color: " << r << " " << g << " " << b << "\n";
            console << "total energy: " << I_total << "\n";
            sendMessage("Update energy1", new MessageOf<pair<float, float>>(UPDATEENERGY_MSG_ID, make_pair(I_total, 0.1026)), parent,
            100, 200);
        } else {
            I_total += myEnergy;
            sendMessage("Update energy2", new MessageOf<pair<float, float>>(UPDATEENERGY_MSG_ID, make_pair(I_total, 0.0f)), parent,
            100, 200);
        }
       
    }
    for (auto child : children) {
        sendMessage("set initial energy", new MessageOf<float>(INITIALENERGY_MSG_ID, I_total),
                    child, 100, 200);
    }
}

void EnergyRegulatorBBBlockCode::handleUpdateEnergyMessage(std::shared_ptr<Message> _msg,
                                                           P2PNetworkInterface *sender) {
    pair<float,float> newValue = *dynamic_cast<MessageOf<pair<float, float>> *>(_msg.get())->getData();
    console << "Received update energy message from " << sender->getConnectedBlockId() << " "
            << I_total << "\n";
    I_total = newValue.first;
    float reduceFactor = newValue.second;
    if (reduceFactor > 0.0f) {
        I_total -= myEnergy;
        updateColor(r - r * reduceFactor, g - g*reduceFactor, b - b*reduceFactor);
        myEnergy = getCurrentConsumption();
        I_total += myEnergy;
        console << "adapted color: " << r << " " << g << " " << b << "\n";
        console << "total energy: " << I_total << "\n";
    }
    if (find(children.begin(), children.end(), sender) != children.end()) {
        if(parent){
            sendMessage("Update energy", new MessageOf<pair<float, float>>(UPDATEENERGY_MSG_ID, make_pair(I_total, reduceFactor)), parent,
                    100, 200);
        } else {
            for (auto child : children) {
                sendMessage("set initial energy", new MessageOf<float>(INITIALENERGY_MSG_ID, I_total),
                            child, 100, 200);
            }
        }
        
    } else if (sender == parent) {
        for (auto child : children) {
            sendMessage("update energy", new MessageOf<pair<float, float>>(UPDATEENERGY_MSG_ID,  make_pair(I_total, reduceFactor)), child,
                        100, 200);
        }
    }
}

void EnergyRegulatorBBBlockCode::onMotionEnd() {
    console << " has reached its destination" << "\n";

    // do stuff
    // ...
}

void EnergyRegulatorBBBlockCode::processLocalEvent(EventPtr pev) {
    std::shared_ptr<Message> message;
    stringstream info;

    // Do not remove line below
    BlinkyBlocksBlockCode::processLocalEvent(pev);

    switch (pev->eventType) {
        case EVENT_ADD_NEIGHBOR: {
            // Do something when a neighbor is added to an interface of the module
            console << "block added!" << "\n";
            AddNeighborEvent *addEvent = static_cast<AddNeighborEvent *>(pev.get());
            uint64_t face = addEvent->face;
            uint64_t oppositeFace = lattice->getOppositeDirection(face);
            console << "Neighbor added on face " << oppositeFace << "\n";
            if (I_total > 0) {
                // Send message to the new neighbor
                sendMessage("set initial energy1",
                            new MessageOf<float>(INITIALENERGY_MSG_ID, I_total),
                            module->getInterface(oppositeFace), 100, 200);
                children.push_back(module->getInterface(oppositeFace));
            }
            break;
        }

        case EVENT_REMOVE_NEIGHBOR: {
            // Do something when a neighbor is removed from an interface of the module
            break;
        }
    }
}

/// ADVANCED BLOCKCODE FUNCTIONS BELOW

void EnergyRegulatorBBBlockCode::onBlockSelected() {
    // Debug stuff:
    cerr << endl << "--- PRINT MODULE " << *module << "---" << endl;
    console << "total energy: " << I_total << "\n";
    if (parent) {
        console << "parent: " << parent->getConnectedBlockId() << "\n";
    }
    for (auto child : children) {
        console << "child: " << child->getConnectedBlockId() << "\n";
    }
}

void EnergyRegulatorBBBlockCode::onAssertTriggered() {
    console << " has triggered an assert" << "\n";

    // Print debugging some info if needed below
    // ...
}

void EnergyRegulatorBBBlockCode::parseUserBlockElements(TiXmlElement *config) {
    const char *attr = config->Attribute("feeding");
    isFeedingBlock = (attr ? Simulator::extractBoolFromString(attr) : false);
    if (isFeedingBlock) {
        std::cout << getId() << " is a feeding block!" << std::endl;  // complete with your code
    }
}

bool EnergyRegulatorBBBlockCode::parseUserCommandLineArgument(int &argc, char **argv[]) {
    /* Reading the command line */
    if ((argc > 0) && ((*argv)[0][0] == '-')) {
        switch ((*argv)[0][1]) {
            // Single character example: -b
            case 'b': {
                cout << "-b option provided" << endl;
                return true;
            } break;

            // Composite argument example: --foo 13
            case '-': {
                string varg = string((*argv)[0] + 2);  // argv[0] without "--"
                if (varg == string("foo")) {           //
                    int fooArg;
                    try {
                        fooArg = stoi((*argv)[1]);
                        argc--;
                        (*argv)++;
                    } catch (std::logic_error &) {
                        stringstream err;
                        err << "foo must be an integer. Found foo = " << argv[1] << endl;
                        throw CLIParsingError(err.str());
                    }

                    cout << "--foo option provided with value: " << fooArg << endl;
                } else
                    return false;

                return true;
            }

            default:
                cerr << "Unrecognized command line argument: " << (*argv)[0] << endl;
        }
    }

    return false;
}

string EnergyRegulatorBBBlockCode::onInterfaceDraw() {
    stringstream trace;
    trace << "Some value " << 123;
    return trace.str();
}
