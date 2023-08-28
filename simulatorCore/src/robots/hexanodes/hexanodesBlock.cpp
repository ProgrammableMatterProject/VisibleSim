/**
 * @file   nodeBlock.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:47:36 2019
 *
 * @brief
 *
 *
 */

#include <iostream>

#include "../../base/buildingBlock.h"
#include "../../utils/trace.h"
#include "../../replay/replayExporter.h"
#include "../../events/events.h"
#include "hexanodesBlock.h"
#include "hexanodesGlBlock.h"
#include "hexanodesWorld.h"
#include "hexanodesSimulator.h"
#include "hexanodesBlock.h"
#include "hexanodesMotionEngine.h"
#include "hexanodesMotionEvents.h"

using namespace std;

//! \namespace Hexanodes
namespace Hexanodes {

HexanodesBlock::HexanodesBlock(int bId, BlockCodeBuilder bcb)
    : BaseSimulator::BuildingBlock(bId, bcb, HHLattice::MAX_NB_NEIGHBORS) {
#ifdef DEBUG_OBJECT_LIFECYCLE
    OUTPUT << "HexanodesBlock constructor" << endl;
#endif
}

HexanodesBlock::~HexanodesBlock() {
    OUTPUT << "HexanodesBlock destructor " << blockId << endl;
}

int HexanodesBlock::getDirection(P2PNetworkInterface *given_interface) const {
    if( !given_interface) {
        return -1;
    }

    for( int i(0); i < HHLattice::MAX_NB_NEIGHBORS; ++i) {
        if(P2PNetworkInterfaces[i] == given_interface) return i;
    }

    return -1;
}

std::ostream& operator<<(std::ostream &stream, HexanodesBlock const& bb) {
    stream << bb.blockId << "\tcolor: " << bb.color;
    return stream;
}

bool HexanodesBlock::getNeighborPos(uint8_t connectorDir,Cell3DPosition &pos) const {
    HexanodesWorld *wrl = getWorld();
        const Vector3D bs = wrl->lattice->gridScale;
        pos = ((HHLattice*)(wrl->lattice))->getNeighborRelativePos(HHLattice::Direction(connectorDir));
        cout << "neighbor pos=" << pos << endl;
        Vector3D realPos(pos[0]*bs[0],pos[1]*bs[1],pos[2]*bs[2],1.0);
        realPos = ((HexanodesGlBlock*)ptrGlBlock)->mat*realPos;
        cout << "neighbor world pos=" << realPos << endl;
        pos = wrl->lattice->worldToGridPosition(realPos);
        cout << "grid neighbor pos=" << pos << endl;

        return wrl->lattice->isInGrid(pos);
}

void HexanodesBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
           << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(
        new AddNeighborEvent(getScheduler()->now(), this,
                             getWorld()->lattice->getOppositeDirection(getDirection(ni)),
                             target->blockId));
}

void HexanodesBlock::removeNeighbor(P2PNetworkInterface *ni) {
#ifdef DEBUG_NEIGHBORHOOD
    OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
           << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
    getScheduler()->schedule(new RemoveNeighborEvent(getScheduler()->now(), this,getWorld()->lattice->getOppositeDirection(getDirection(ni))));
}

bool HexanodesBlock::hasANeighbor(HHLattice::Direction n, bool groundIsNeighbor) const {
    return hasANeighbor(getInterface(n),groundIsNeighbor);
}

bool HexanodesBlock::hasANeighbor(P2PNetworkInterface *p2p, bool groundIsNeighbor) const {
    Cell3DPosition p = getPosition(p2p);
    if(p2p->connectedInterface) {
        return true;
    } else if (groundIsNeighbor && (p[2]<0)) {
        return true;
    }
    return false;
}

Cell3DPosition HexanodesBlock::getPosition(HHLattice::Direction d) const {
    World *wrl = getWorld();
    const vector<Cell3DPosition>& nCells = wrl->lattice->getRelativeConnectivity(position);
    return position + nCells[d];
}

// PTHY: TODO: Can be genericized in BuildingBlocks
Cell3DPosition HexanodesBlock::getPosition(P2PNetworkInterface *p2p) const{
    return getPosition((HHLattice::Direction)getDirection(p2p));
}

void HexanodesBlock::setPosition(const Cell3DPosition &p) {
    setPositionAndOrientation(p, orientationCode);
}

void HexanodesBlock::setPositionAndOrientation(const Cell3DPosition &pos, uint8_t code) {
    orientationCode = code;
    position = pos;

    //cout << "setPositionAndOrientation:" << pos << endl;
    Matrix M=getMatrixFromPositionAndOrientation(pos,code);
    //cout << M << endl;
    getWorld()->updateGlData(this,M);
    getWorld()->updateGlData(this,position);

    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writePositionUpdate(getScheduler()->now(),
                                                           blockId, position, orientationCode);
}

uint8_t HexanodesBlock::getOrientationFromMatrix(const Matrix &mat) {
    static uint8_t tab[3][3]={{4,2,4},{3,4,1},{4,0,4}};
    Vector3D V=mat * Vector3D(1,0,0);
    int cx=(int)(V[0]+1.5);
    int cy=(int)(V[1]+1.5);

    /*1,0 -> 0
    0,1 -> 1
    -1,0 -> 2
    0,-1 -> 3*/

    //OUTPUT << "result =" << current << endl;
    return (tab[cx][cy]);
}

Matrix HexanodesBlock::getMatrixFromPositionAndOrientation(const Cell3DPosition &pos, uint8_t code) {
    uint8_t orientation = code;

    Matrix M1,M2,M;
    M1.setRotationZ(-orientation*60.0);
    Vector3D V=getWorld()->lattice->gridToWorldPosition(pos);//-Vector3D(-12.5,-12.5,0);
    M2.setTranslation(V);
    M = M2*M1;
    return M;
}

bool HexanodesBlock::canMoveTo(const Cell3DPosition& dest) const {
    vector<HexanodesMotion*> motions =
        Hexanodes::getWorld()->getAllMotionsForModule(const_cast<HexanodesBlock*>(this));
    for (const auto* motion : motions) {
        if (motion->getFinalPos(position) == dest)
            return true;
    }

    return false;
}

bool HexanodesBlock::moveTo(const Cell3DPosition& dest) {
    vector<HexanodesMotion*> motions = Hexanodes::getWorld()->getAllMotionsForModule(this);

    for (const auto* motion : motions) {
        if (motion->getFinalPos(position) == dest) {
            getScheduler()->schedule(
                new HexanodesMotionStartEvent(getScheduler()->now(), this,
                                              dest, motion->getToConId()));
            return true;
        }
    }
    return false;
}

    vector<pair<Cell3DPosition, uint8_t>> HexanodesBlock::getAllMotions() const {
        vector<pair<Cell3DPosition, uint8_t>> res;
        vector<HexanodesMotion *> motions =
                Hexanodes::getWorld()->getAllMotionsForModule(const_cast<HexanodesBlock *>(this));
        for (const auto *motion: motions) {
            res.emplace_back(pair<Cell3DPosition, uint8_t>(motion->getFinalPos(position),
                                                           motion->getFinalOrientation(orientationCode)));
        }
        return res;
    }

void HexanodesBlock::setDisplayedValue(int n) {
    static_cast<HexanodesGlBlock*>(ptrGlBlock)->setDisplayedValue(n);
    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeDisplayUpdate(getScheduler()->now(), blockId, n);
}

}
