/*
 * SmartBlocksBlock.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include "smartBlocksBlock.h"
#include "smartBlocksWorld.h"
#include "../../motion/translationEvents.h"
#include "../../replay/replayExporter.h"

using namespace std;

namespace SmartBlocks {

    SmartBlocksBlock::SmartBlocksBlock(int bId, BlockCodeBuilder bcb)
            : BaseSimulator::BuildingBlock(bId, bcb, SLattice::MAX_NB_NEIGHBORS) {
        orientationCode = 0;
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << "SmartBlocksBlock #" << bId << " constructor" << endl;
#endif
    }

    SmartBlocksBlock::~SmartBlocksBlock() {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << "SmartBlocksBlock #" << blockId << " destructor" << endl;
#endif
    }

    P2PNetworkInterface *SmartBlocksBlock::getP2PNetworkInterfaceByRelPos(const Cell3DPosition &pos) const {
        if (pos[0] == -1) return P2PNetworkInterfaces[SLattice::West];
        else if (pos[0] == 1) return P2PNetworkInterfaces[SLattice::East];
        else if (pos[1] == -1) return P2PNetworkInterfaces[SLattice::South];
        else if (pos[1] == 1) return P2PNetworkInterfaces[SLattice::North];

        return NULL;
    }

    int SmartBlocksBlock::getDirection(P2PNetworkInterface *given_interface) const {
        /*if( !given_interface) {
          return SLattice::Direction(0);
          }*/
        for (int i(SLattice::North); i <= SLattice::West; ++i) {
            P2PNetworkInterface *p2p = P2PNetworkInterfaces[i];
            if (p2p == given_interface) {
                return SLattice::Direction(i);
            }
        }
        assert(0);            // should never get here
    }

    Cell3DPosition SmartBlocksBlock::getPosition(SLattice::Direction d) const {
        Cell3DPosition p = position;

        switch (d) {
            case SLattice::North :
                p.pt[1]++;
                break;
            case SLattice::East :
                p.pt[0]--;
                break;
            case SLattice::South :
                p.pt[1]--;
                break;
            case SLattice::West :
                p.pt[0]++;
                break;
            default :
                cerr << "error: getPosition: Undefined position " << d << endl;
                exit(EXIT_FAILURE);
        }


        return p;
    }

    P2PNetworkInterface *SmartBlocksBlock::getP2PNetworkInterfaceByDestBlockId(bID id) const {
        int i = 0;
        while (i < 4 && (P2PNetworkInterfaces[i]->connectedInterface == NULL
                         || P2PNetworkInterfaces[i]->connectedInterface->hostBlock->blockId != id)) {
            i++;
        }
        return (i < 4 ? P2PNetworkInterfaces[i] : NULL);
    }


    void SmartBlocksBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock *target) {
#ifdef DEBUG_NEIGHBORHOOD
        OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
               << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
        getScheduler()->schedule(
                new AddNeighborEvent(getScheduler()->now(), this,
                                     getWorld()->lattice->getOppositeDirection(getDirection(ni)), target->blockId));
    }

    void SmartBlocksBlock::removeNeighbor(P2PNetworkInterface *ni) {
#ifdef DEBUG_NEIGHBORHOOD
        OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
               << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
        getScheduler()->schedule(
                new RemoveNeighborEvent(getScheduler()->now(), this,
                                        getWorld()->lattice->getOppositeDirection(getDirection(ni))));
    }

    bool SmartBlocksBlock::canMoveTo(const Cell3DPosition &dest) const {
        Lattice *lattice = getWorld()->lattice;

        return lattice->isInGrid(dest)
               and lattice->cellsAreAdjacent(position, dest)
               and lattice->isFree(dest);
    }

    bool SmartBlocksBlock::moveTo(const Cell3DPosition &dest) {
        if (not canMoveTo(dest)) return false;

        if (ReplayExporter::isReplayEnabled())
            ReplayExporter::getInstance()->writeMotion(getScheduler()->now(), blockId, 1002000, dest, position); // BPi : duration is currently fixed due to translation event implementation

        getScheduler()->schedule(new TranslationStartEvent(getScheduler()->now(), this, dest));
        return true;
    }

    vector<pair<Cell3DPosition, uint8_t>> SmartBlocksBlock::getAllMotions() const {
        vector<pair<Cell3DPosition, uint8_t>> res;
        Lattice *lattice = getWorld()->lattice;
        auto neighborhood = lattice->getNeighborhood(position);
        for (auto &n:neighborhood) {
            if (canMoveTo(n)) res.emplace_back(pair<Cell3DPosition,uint8_t>(n,orientationCode));
        }
        return res;
    }


    void SmartBlocksBlock::setDisplayedValue(uint16_t n) {
        static_cast<SmartBlocksGlBlock *>(ptrGlBlock)->setDisplayedValue(n);
        if (ReplayExporter::isReplayEnabled())
            ReplayExporter::getInstance()->writeDisplayUpdate(getScheduler()->now(), blockId, n);
    }

    void SmartBlocksBlock::serialize(std::ofstream &bStream) {
        bStream.write((char *) &blockId, sizeof(ReplayTags::u4));
        bStream.write((char *) &position, 3 * sizeof(ReplayTags::u2));
        bStream.write((char *) &orientationCode, sizeof(ReplayTags::u1));
        ReplayTags::u1 u1color[3];
        for (std::size_t i = 0; i < 3; i++) {
            u1color[i] = color[i];
            /*if (u1color[i] < 0) u1color[i] = 0;
            else if (u1color[i] > 255) u1color[i] = 255;*/
        }
        bStream.write((char *) &u1color, 3 * sizeof(ReplayTags::u1));
        ReplayTags::u2 displayValue = dynamic_cast<SmartBlocksGlBlock *>(ptrGlBlock)->displayedValue;
        bStream.write((char *) &displayValue, sizeof(ReplayTags::u2));
    }

    void SmartBlocksBlock::serialize_cleartext(std::ofstream &dbStream) {
        dbStream << (int) blockId << ";"
                 << position[0] << "," << position[1] << "," << position[2] << "," << (int) orientationCode << ";"
                 << (int) (color[0] * 255) << "," << (int) (color[1] * 255) << "," << (int) (color[2] * 255) << ";"
                 << dynamic_cast<SmartBlocksGlBlock *>(ptrGlBlock)->displayedValue << endl;
    }

}
