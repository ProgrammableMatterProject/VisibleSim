/*
 * buildingBlock.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom, bpiranda
 */

#include <iostream>
#include "buildingBlock.h"
#include "world.h"
#include "simulator.h"
#include "../events/scheduler.h"
#include "../utils/trace.h"
#include "../clock/clock.h"
#include "../replay/replayExporter.h"

using namespace std;

namespace BaseSimulator {

    bID BuildingBlock::nextId = 0;
    bool BuildingBlock::userConfigHasBeenParsed = false;

//===========================================================================================================
//
//          BuildingBlock  (class)
//
//===========================================================================================================

    BuildingBlock::BuildingBlock(int bId, BlockCodeBuilder bcb, int nbInterfaces) {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << "BuildingBlock constructor (id:" << nextId << ")" << endl;
#endif

        if (bId < 0) {
            blockId = nextId;
            nextId++;
        } else {
            blockId = bId;
        }

        state.store(ALIVE);
        clock = new PerfectClock();

        ruint seed = Simulator::getSimulator()->getRandomUint();
        seed *= bId;
        generator = uintRNG(seed);

        buildNewBlockCode = bcb;

        if (utils::StatsIndividual::enable) {
            stats = new StatsIndividual();
        }

        for (int i = 0; i < nbInterfaces; i++) {
            P2PNetworkInterfaces.push_back(new P2PNetworkInterface(this));
        }

        //setDefaultHardwareParameters();

        blockCode = (BaseSimulator::BlockCode*)bcb(this);

        // Parse user configuration from configuration file, only performed once
        if (!userConfigHasBeenParsed) {
            userConfigHasBeenParsed = true;
            blockCode->parseUserElements(Simulator::getSimulator()->getConfigDocument());
        }

    }

    BuildingBlock::~BuildingBlock() {
        delete blockCode;
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << "BuildingBlock destructor" << endl;
#endif

        delete clock;
        delete stats;

        for (P2PNetworkInterface *p2p : P2PNetworkInterfaces)
            delete p2p;
    }

    short BuildingBlock::getInterfaceId(const P2PNetworkInterface* itf) const {
        for (unsigned short int i = 0; i < P2PNetworkInterfaces.size(); i++) {
            if (itf == P2PNetworkInterfaces[i]) return i;
        }
        return -1;
    }

    uint8_t BuildingBlock::getInterfaceBId(const P2PNetworkInterface* itf) const {
        for (size_t i = 0; i < P2PNetworkInterfaces.size(); i++) {
            if (itf == P2PNetworkInterfaces[i]) return static_cast<uint8_t>(i);
        }
        return static_cast<uint8_t>(255);
    }

    bool BuildingBlock::addP2PNetworkInterfaceAndConnectTo(BuildingBlock *destBlock) {
        P2PNetworkInterface *ni1, *ni2;
        ni1 = nullptr;
        ni2 = nullptr;
        if (!getP2PNetworkInterfaceByBlockRef(destBlock)) {
            // creation of the new network interface
            OUTPUT << "adding a new interface to block " << destBlock->blockId << endl;
            ni1 = new P2PNetworkInterface(this);
            P2PNetworkInterfaces.push_back(ni1);
        }

        if (!destBlock->getP2PNetworkInterfaceByBlockRef(this)) {
            // creation of the new network interface
            OUTPUT << "adding a new interface to block " << this->blockId << endl;
            ni2 = new P2PNetworkInterface(destBlock);
            destBlock->P2PNetworkInterfaces.push_back(ni2);
        }

        if (ni1!=nullptr && ni2!=nullptr) {
            ni1->connect(ni2);
            return (true);
        } else {
            OUTPUT << "*** ERROR *** could not connect the new interfaces" << endl;
        }
        return false;
    }

    bool BuildingBlock::addP2PNetworkInterfaceAndConnectTo(int destBlockId) {
        // if the link is not in the list
        BuildingBlock *destBlock = BaseSimulator::getWorld()->getBlockById(destBlockId);
        if (!getP2PNetworkInterfaceByBlockRef(destBlock)) {
            // creation of the new network interface
            P2PNetworkInterface* ni1 = new P2PNetworkInterface(this);
            P2PNetworkInterfaces.push_back(ni1);
            // if the corresponding interface exists in the connected block, we link the two interfaces
            if (destBlock->addP2PNetworkInterfaceAndConnectTo(this)) {
                P2PNetworkInterface* ni2 = destBlock->getP2PNetworkInterfaceByBlockRef(this);
                ni1->connect(ni2);
            }
        }
        return false;
    }

    P2PNetworkInterface *BuildingBlock::getP2PNetworkInterfaceByBlockRef(BuildingBlock *destBlock) const {
        for(P2PNetworkInterface *p2p : P2PNetworkInterfaces) {
            if (p2p->connectedInterface) {
                if (p2p->connectedInterface->hostBlock == destBlock) {
                    return p2p;
                }
            }
        }
        return nullptr;
    }

    P2PNetworkInterface*BuildingBlock::getP2PNetworkInterfaceByDestBlockId(bID destBlockId) const {
        for(P2PNetworkInterface *p2p : P2PNetworkInterfaces) {
            if (p2p->connectedInterface) {
                if (p2p->connectedInterface->hostBlock->blockId == destBlockId) {
                    return p2p;
                }
            }
        }
        return nullptr;
    }

    bool BuildingBlock::hasNeighbor(int dir) {
        P2PNetworkInterface *p2p = P2PNetworkInterfaces[dir];
        if (p2p == nullptr) return false;
        if (!p2p->connectedInterface) return false;
        return p2p->connectedInterface->hostBlock != nullptr;
    }

    uint8_t BuildingBlock::getNbNeighbors() const {
        uint8_t n = 0;

        for (const P2PNetworkInterface* p2p : P2PNetworkInterfaces) {
            n+=p2p->isConnected();
        }
        return n;
    }

    vector<BuildingBlock*> BuildingBlock::getNeighbors() const {
        vector<BuildingBlock*> res;
        for (P2PNetworkInterface* p2p:P2PNetworkInterfaces) {
            if (p2p->isConnected()) {
                res.push_back(p2p->connectedInterface->hostBlock);
            }
        }
        return res;
    }


    bool BuildingBlock::getNeighborPos(uint8_t connectorId,Cell3DPosition &pos) const {
        Lattice *lattice = getWorld()->lattice;
        vector<Cell3DPosition> nCells = lattice->getRelativeConnectivity(position);
        pos = position + nCells[connectorId];
        return lattice->isInGrid(pos);
    }


    void BuildingBlock::scheduleLocalEvent(EventPtr pev) {
        localEventsList.push_back(pev);

        if (localEventsList.size() == 1) {
            Time date;
            date = max(pev->date,this->blockCode->availabilityDate); // WARNING: is blockCode->availabilityDate considered?
            if (date < getScheduler()->now()) date=getScheduler()->now();
            getScheduler()->schedule(new ProcessLocalEvent(date,this));
        }
        return;
    }

    void BuildingBlock::processLocalEvent() {
        EventPtr pev;

        if (localEventsList.size() == 0) {
            cerr << "*** ERROR *** The local event list should not be empty !!" << endl;
            getScheduler()->trace("*** ERROR *** The local event list should not be empty !!");
            exit(EXIT_FAILURE);
        }
        pev = localEventsList.front();
        localEventsList.pop_front();

        try {
            blockCode->processLocalEvent(pev);
        } catch(VisibleSimException const& e) {
            cerr << "exception raised! (see below)" << endl;
            cerr << e.what() << endl;
            awaitKeyPressed();
            throw;
        }

        if (pev->eventType == EVENT_NI_RECEIVE ) {
            utils::StatsIndividual::decIncommingMessageQueueSize(stats);
        }

        if (blockCode->availabilityDate < getScheduler()->now()) blockCode->availabilityDate = getScheduler()->now();
        if (localEventsList.size() > 0) {
            getScheduler()->schedule(new ProcessLocalEvent(blockCode->availabilityDate,this));
        }
    }

    void BuildingBlock::setColor(int idColor) {
        color = Colors[idColor%NB_COLORS];
        // getWorld()->updateGlData(this); // separate update color and update position
        getWorld()->updateGlData(this,color);

        if (ReplayExporter::isReplayEnabled())
            ReplayExporter::getInstance()->writeColorUpdate(getScheduler()->now(), blockId, color);
    }

    void BuildingBlock::setColor(const Color &c) {
        if (state.load() >= ALIVE) {
            color = c;
            // getWorld()->updateGlData(this); // separate update color and update position
            getWorld()->updateGlData(this,color);

            if (ReplayExporter::isReplayEnabled())
                ReplayExporter::getInstance()->writeColorUpdate(getScheduler()->now(),blockId, color);
        }
    }

    void BuildingBlock::setPosition(const Cell3DPosition &p) {
        if (state.load() >= ALIVE) {
            position = p;
            // getWorld()->updateGlData(this); // separate update color and update position
            getWorld()->updateGlData(this,p);

            /*if (ReplayExporter::isReplayEnabled())
                ReplayExporter::getInstance()->writePositionUpdate(getScheduler()->now(),
                                                                   blockId, position, orientationCode);*/
        }
    }


    void BuildingBlock::tap(Time date, int face) {
        OUTPUT << "tap scheduled" << endl;
        getScheduler()->schedule(new TapEvent(date, this, (uint8_t)face));
    }

    ruint BuildingBlock::getRandomUint() {
        return generator();
    }

    void BuildingBlock::setClock(Clock *c) {
        if (clock != nullptr) {
            delete clock;
        }
        clock = c;
    }

    Time BuildingBlock::getLocalTime(Time simTime) const {
        if (clock == nullptr) {
            cerr << "device has no internal clock" << endl;
            return 0;
        }
        return clock->getTime(simTime);
    }

    Time BuildingBlock::getLocalTime() const {
        if (clock == nullptr) {
            cerr << "device has no internal clock" << endl;
            return 0;
        }
        return clock->getTime();
    }

    Time BuildingBlock::getSimulationTime(Time localTime) const {
        if (clock == nullptr) {
            cerr << "device has no internal clock" << endl;
            return localTime;
        }
        return clock->getSimulationTime(localTime);
    }

    std::ostream& operator<<(std::ostream &stream, BuildingBlock const& bb) {
        stream << "#" << bb.blockId;
        return stream;
    }

    unsigned short BuildingBlock::getNeighborIDForFace(int faceNum) const {
        short nodeID = P2PNetworkInterfaces[faceNum]->getConnectedBlockId();

        return nodeID > 0  ? (unsigned short)nodeID : 0;
    }

    int BuildingBlock::getFaceForNeighborID(int nId) const {
        for (unsigned int face = 0; face < P2PNetworkInterfaces.size(); face++) {
            if (nId == getNeighborIDForFace(face))
                return face;
        }

        return -1;
    }

    void BuildingBlock::serialize(std::ofstream &bStream) {
        bStream.write((char*)&blockId, sizeof(ReplayTags::u4));
        bStream.write((char*)&position, 3*sizeof(ReplayTags::u2));
        bStream.write((char*)&orientationCode, sizeof(ReplayTags::u1));
        ReplayTags::u1 u1color[3];
        for (std::size_t i=0;i<3; i++) {
            u1color[i] = color[i];
            /*if (u1color[i]<0) u1color[i]=0;
            else if (u1color[i]>255) u1color[i]=255;*/
        }
        bStream.write((char*)&u1color, 3*sizeof(ReplayTags::u1));
    }

    void BuildingBlock::serialize_cleartext(std::ofstream &dbStream) {
        dbStream << (int)blockId << ";"
                 << position[0] << "," << position[1] << ","<< position[2] << "," << (int) orientationCode << ";"
                 << (int)(color[0]*255) << "," << (int)(color[1]*255) << ","<< (int)(color[2]*255) << endl;
    }

} // BaseSimulator namespace
