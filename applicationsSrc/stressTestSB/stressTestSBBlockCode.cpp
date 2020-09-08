#include "stressTestSBBlockCode.hpp"
#include "base/simulator.h"
#include <experimental/iterator>

using namespace SmartBlocks;
static unsigned int nbUnmarked;

void printProgress() {
    static unsigned int lastPrint=nbUnmarked;
    bool active=false;
    if (nbUnmarked>10000000) {
        active=nbUnmarked<=lastPrint;
        if (active) lastPrint=(nbUnmarked/1000000-1)*1000000;
    } else if (nbUnmarked>1000000) {
        active=nbUnmarked<=lastPrint;
        if (active) lastPrint=(nbUnmarked/100000-1)*100000;
    } else if (nbUnmarked>100000) {
        active=nbUnmarked<=lastPrint;
        if (active) lastPrint=(nbUnmarked/10000-1)*10000;
    } else if (nbUnmarked>10000) {
        active=nbUnmarked<=lastPrint;
        if (active) lastPrint=(nbUnmarked/1000-1)*1000;
    } else if (nbUnmarked>1000) {
        active=nbUnmarked<=lastPrint;
        if (active) lastPrint=(nbUnmarked/100-1)*100;
    } else if (nbUnmarked>100) {
        active=nbUnmarked<=lastPrint;
        if (active) lastPrint=(nbUnmarked/10-1)*10;
    } else {
        active=true;
    }
    if (active) cout << "#" << nbUnmarked <<endl;
}

template <typename T>
void print(std::vector<T> const &input) {
    std::copy(input.begin(),
              input.end(),
              std::experimental::make_ostream_joiner(std::cout, ", "));
    cout << endl;
}

StressTestSBBlockCode::StressTestSBBlockCode(SmartBlocksBlock *host) : SmartBlocksBlockCode(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;

    // Registers a callback (handleSampleMessage) to the message of type SAMPLE_MSG_ID
    addMessageEventFunc2(ACTIVATION_MSG_ID,
                         std::bind(&StressTestSBBlockCode::handleActivationMessage, this,
                         std::placeholders::_1, std::placeholders::_2));

    // Set the module pointer
    module = static_cast<SmartBlocksBlock*>(hostBlock);
    lattice = BaseSimulator::getWorld()->lattice;
}

void StressTestSBBlockCode::startup() {
    if (module->blockId==1) {
        nbUnmarked=SmartBlocksWorld::getWorld()->getNbBlocks();
        cout << "#block =" << nbUnmarked << endl;
        printProgress();
    }

    rng.seed(BaseSimulator::Simulator::getSimulator()->getCmdLine().getSimulationSeed());

    initLockedCells();

    if (nbUnmarked<1000) module->setDisplayedValue(module->blockId);
    else module->disableDisplay();
    // Leader initiates activation
    if (isLeader) {
        nbUnmarked--;
        activated=true;
        module->setColor(BLUE);
        setLockedCell(module->position, true);
        sendMessageToAllNeighbors("Activate", new Message(ACTIVATION_MSG_ID),100,200,0);
        wait();
    } else {
        //hostBlock->setColor(YELLOW);
        setLockedCell(module->position, true);
    }
}

void StressTestSBBlockCode::handleActivationMessage(std::shared_ptr<Message> _msg,
                                                    P2PNetworkInterface* sender) {
    if (not activated && not isMoving) {
        activated = true;
        module->setColor(BLUE);
        nbUnmarked--;
		/*		cout << nbUnmarked << endl;
        console << " Received ACTIVATE from "
                << sender->getConnectedBlockId() << "(" << nbUnmarked << ")\n";
*/
		printProgress();
        // Broadcast to all neighbors but ignore sender
        /*sendMessageToAllNeighbors("Activate", new Message(ACTIVATION_MSG_ID),100,200,
          1,sender); // ignore sender*/
        wait();
    }
}

void StressTestSBBlockCode::onMotionEnd() {
    isMoving=false;
    //console << " finished random motion to " << module->position << "\n";
    setLockedCell(lastPos, false);
    sendMessageToAllNeighbors("Activate", new Message(ACTIVATION_MSG_ID),100,200,0);

    wait();
}

void StressTestSBBlockCode::processLocalEvent(std::shared_ptr<Event> pev) {
    std::shared_ptr<Message> message;
    stringstream info;

    // Do not remove line below
    SmartBlocksBlockCode::processLocalEvent(pev);

    switch (pev->eventType) {
        case EVENT_ADD_NEIGHBOR: {
            // Do something when a neighbor is added to an interface of the module
            break;
        }

        case EVENT_REMOVE_NEIGHBOR: {
            // Do something when a neighbor is removed from an interface of the module
            break;
        }

        case EVENT_INTERRUPTION: {
            std::shared_ptr<InterruptionEvent> itev =
                std::static_pointer_cast<InterruptionEvent>(pev);

            switch(itev->mode) {
                case AGITATE_IT_ID:
                    //console << " agitate" << "\n";
                    if (!isMoving && nbUnmarked>0) {
                        if (not randomWalk()) { // Wait .5s and try moving again
                            wait();
                        } else {
                            module->setColor(BLUE);
                        }
                    }
                    break;
            }
        } break;
    }
}

bool StressTestSBBlockCode::randomWalk() {
    // Get list of possible destinations
    const vector<Cell3DPosition>& freeNeighborCells =
        lattice->getFreeNeighborCells(module->position);

    // Prune unreachable destinations
    vector<Cell3DPosition> candidates;
    std::copy_if(freeNeighborCells.begin(), freeNeighborCells.end(),
                 std::back_inserter(candidates),
                 [this](const Cell3DPosition& p){
                     return not isLockedCell(p) and module->canMoveTo(p); });

    if (candidates.empty()) {
        module->setColor(RED);
        module->disableDisplay();
        return false;
    }
    module->setDisplayedValue(candidates.size());

    // Randomly pick one candidate
    std::random_shuffle ( candidates.begin(), candidates.end() );

    // Lock cell and move
    const Cell3DPosition& dest = candidates.front();
    lastPos = module->position;
    setLockedCell(dest, true);
    //console << "Moveto" << dest << "\n";
    isMoving = true;
    module->moveTo(dest);

    return true;
}

void StressTestSBBlockCode::wait() {
    std::uniform_int_distribution<std::mt19937::result_type> u500(450000, 550000);
    getScheduler()->schedule(new InterruptionEvent(scheduler->now() + u500(rng),
                                                   this->module, AGITATE_IT_ID));
}

void StressTestSBBlockCode::initLockedCells() {
    if (lockedCells == nullptr) {
        const Cell3DPosition& gs = lattice->gridSize;
        int n = gs.pt[0]*gs.pt[1]*gs.pt[2];
        lockedCells = new bool[n];
        // initialisation of lockedCells with value false
        bool *ptr = lockedCells;
        while (n--) {
            *ptr++ = false;
        }
    }
}

void StressTestSBBlockCode::onBlockSelected() {
    // Get list of possible destinations
    const vector<Cell3DPosition>& freeNeighborCells =
        lattice->getFreeNeighborCells(module->position);

    print(freeNeighborCells);

    // Prune unreachable destinations
    vector<Cell3DPosition> candidates;
    std::copy_if(freeNeighborCells.begin(), freeNeighborCells.end(),
                 std::back_inserter(candidates),
                 [this](const Cell3DPosition& p){
                     return not isLockedCell(p) and module->canMoveTo(p); });

    print(candidates);

    if (candidates.empty()) cerr << "no candidates" << endl;
    else {
        // Randomly pick one candidate
        std::random_shuffle ( candidates.begin(), candidates.end() );

        cout << "Pick: " << candidates.front() << endl;
    }
}

void StressTestSBBlockCode::parseUserBlockElements(TiXmlElement *config) {
    const char *attr = config->Attribute("leader");
    isLeader=(attr!=nullptr);
    if (isLeader) cout << module->blockId << " is leader!" << endl;
}


void StressTestSBBlockCode::onGlDraw() {
    /*static const float color[4]={0.2f,0.2f,0.2f,1.0f};
      if (lockedCells) {
      const Cell3DPosition& gs = lattice->gridSize;
      const Vector3D gl = lattice->gridScale;
      bool *ptr=lockedCells;
      glDisable(GL_TEXTURE);
      glMaterialfv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE,color);
      for (int iy=0; iy<gs[1]; iy++) {
      for (int ix=0; ix<gs[0]; ix++) {
      if (*ptr) {
      glPushMatrix();
      glTranslatef((ix+0.5f)*gl[0],(iy+0.5f)*gl[1],10.0);
      glScalef(3,3,10);
      glutSolidCube(1.0);
      glPopMatrix();
      }
      ptr++;
      }
      }

      }*/
}

string StressTestSBBlockCode::onInterfaceDraw() {
    return "#Unactivated:" + to_string(nbUnmarked);
}
