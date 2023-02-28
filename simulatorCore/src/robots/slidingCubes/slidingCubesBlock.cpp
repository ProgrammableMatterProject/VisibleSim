/*
 * slidingCubesBlock.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>

#include "slidingCubesBlock.h"
#include "slidingCubesWorld.h"
#include "slidingCubesSimulator.h"
#include "../../grid/lattice.h"
#include "../../utils/trace.h"
#include "../../motion/teleportationEvents.h"

using namespace std;

namespace SlidingCubes {

    SlidingCubesBlock::SlidingCubesBlock(int bId, BlockCodeBuilder bcb)
            : BaseSimulator::BuildingBlock(bId, bcb, 6) {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << "SlidingCubesBlock constructor" << endl;
#endif
        orientationCode = 0;
    }

    SlidingCubesBlock::~SlidingCubesBlock() {
        OUTPUT << "SlidingCubesBlock destructor " << blockId << endl;
    }

    void SlidingCubesBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock *target) {
#ifdef DEBUG_NEIGHBORHOOD
        OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
               << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
        getScheduler()->schedule(
                new AddNeighborEvent(getScheduler()->now(), this,
                                     getWorld()->lattice->getOppositeDirection(getDirection(ni)), target->blockId));
    }

    void SlidingCubesBlock::setPositionAndOrientation(const Cell3DPosition &pos, uint8_t code) {
        orientationCode = code;
        position = pos;

        Matrix M = getMatrixFromPositionAndOrientation(pos, code);
        getWorld()->updateGlData(this, M);
        getWorld()->updateGlData(this, position);
        if (ReplayExporter::isReplayEnabled())
            ReplayExporter::getInstance()->writePositionUpdate(getScheduler()->now(),
                                                               blockId, position, orientationCode);

    }

    uint8_t SlidingCubesBlock::getOrientationFromMatrix(const Matrix &mat) {
        static const Vector3D Vref[4] = {Vector3D(0, 0, 1, 0), Vector3D(0, 1, 0, 0), Vector3D(0, 0, -1, 0),
                                         Vector3D(0, -1, 0, 0)};
        Vector3D x(1.0, 0.0, 0.0, 0.0); // Vector3D X
        Vector3D v;
        Matrix mat_1;
        mat.inverse(mat_1);

        uint8_t current = -1;
        double psmax = -1;
        for (int i = 0; i < 6; i++) {
            x.set(tabConnectorPositions[i], 3);
            v = mat * x;
            //OUTPUT << "connector #" << i << ":" << v << endl;
            if (v[0] > psmax) {
                current = i;
                psmax = v[0];
            }
        }
        // orientation autour du connecteur
        Vector3D z(0.0, 0.0, 1.0, 0.0); // Vector3D X
        //z.set(tabConnectorV[i],3);
        Matrix M1, M2, M3, M;
        M1.setRotationZ(tabOrientationAngles[current][2]);
        M2.setRotationY(tabOrientationAngles[current][1]);
        M3.setRotationX(tabOrientationAngles[current][0]);
        M = M2 * M1;
        M1 = M3 * M;
        /*M2.setTranslation(getWorld()->lattice->gridToWorldPosition(pos));
        M = M2 * M1;*/

        M = M1 * mat_1;
        v = M * z;
        psmax = -1;
        uint8_t code = 0;
        for (int i = 0; i < 4; i++) {
            auto ps = v * Vref[i];
            if (ps > psmax) {
                psmax = ps;
                code = i;
            }
        }
        return current + code * 6;
    }

    Matrix SlidingCubesBlock::getMatrixFromPositionAndOrientation(const Cell3DPosition &pos,
                                                                  uint8_t code) {
        uint8_t orientation = code % 6;
        uint8_t up = code / 6;

        Matrix M1, M2, M3, M;
        M1.setRotationZ(tabOrientationAngles[orientation][2]);
        M2.setRotationY(tabOrientationAngles[orientation][1]);
        M3.setRotationX(tabOrientationAngles[orientation][0] + up * 90.0);
        M = M2 * M1;
        M1 = M3 * M;
        M2.setTranslation(getWorld()->lattice->gridToWorldPosition(pos));
        M = M2 * M1;
        return M;
    }

    void SlidingCubesBlock::removeNeighbor(P2PNetworkInterface *ni) {
#ifdef DEBUG_NEIGHBORHOOD
        OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
               << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
        getScheduler()->schedule(
                new RemoveNeighborEvent(getScheduler()->now(), this,
                                        getWorld()->lattice->getOppositeDirection(getDirection(ni))));
    }

    int SlidingCubesBlock::getDirection(P2PNetworkInterface *given_interface) const {
        if (!given_interface) {
            return -1;
        }
        for (int i(0); i < 6; ++i) {
            if (P2PNetworkInterfaces[i] == given_interface) return i;
        }
        return -1;
    }

    bool SlidingCubesBlock::getNeighborPos(uint8_t connectorID, Cell3DPosition &pos) const {
        Vector3D realPos;

        auto *wrl = getWorld();
        const Vector3D bs = wrl->lattice->gridScale;
        realPos.set(tabConnectorPositions[connectorID], 3, 1);
        realPos *= bs;
        realPos.set(3, 1.0); // A vérifier
        realPos = static_cast<SlidingCubesGlBlock *>(ptrGlBlock)->mat * realPos;
        pos = wrl->lattice->worldToGridPosition(realPos);
        return wrl->lattice->isInGrid(pos);
    }

    P2PNetworkInterface *SlidingCubesBlock::getP2PNetworkInterfaceByRelPos(const Cell3DPosition &pos) const {
        if (pos[0] == -1) return P2PNetworkInterfaces[SCLattice2::MinusX];
        else if (pos[0] == 1) return P2PNetworkInterfaces[SCLattice2::PlusX];
        else if (pos[1] == -1) return P2PNetworkInterfaces[SCLattice2::MinusY];
        else if (pos[1] == 1) return P2PNetworkInterfaces[SCLattice2::PlusY];
        else if (pos[2] == -1) return P2PNetworkInterfaces[SCLattice2::MinusZ];
        else if (pos[2] == 1) return P2PNetworkInterfaces[SCLattice2::PlusZ];

        return NULL;
    }

    std::ostream &operator<<(std::ostream &stream, SlidingCubesBlock const &bb) {
        stream << bb.blockId << "\tcolor: " << bb.color;
        return stream;
    }


    bool SlidingCubesBlock::canMoveTo(const Cell3DPosition &dest) const {
        auto wrl = getWorld();
        vector<SlidingCubesMotionRule *> tab = wrl->getMotionRules()->getValidMotionList(this);
        if (tab.empty()) return false;
        auto itRule = tab.begin();
        Cell3DPosition finalPos;
        short finalOrient = 0;
        bool found = false;
        while (itRule != tab.end() && !found) {
            (*itRule)->getFinalPositionAndOrientation(this, finalPos, finalOrient);
            found = finalPos == dest;
            itRule++;
        }
        return found;
    }

    bool SlidingCubesBlock::moveTo(const Cell3DPosition &dest) {
        auto wrl = getWorld();
        vector<SlidingCubesMotionRule *> tab = wrl->getMotionRules()->getValidMotionList(this);
        if (tab.empty()) return false;
        auto itRule = tab.begin();
        Cell3DPosition finalPos;
        short finalOrient = 0;
        bool found = false;
        while (itRule != tab.end() && !found) {
            (*itRule)->getFinalPositionAndOrientation(this, finalPos, finalOrient);
            found = finalPos == dest;
            itRule++;
        }
        if (found) {
            auto scheduler = getScheduler();
            scheduler->schedule(new TeleportationStartEvent(scheduler->now() + 1000000, this, finalPos, finalOrient));
        }
        return found;
    }

    vector<pair<Cell3DPosition,uint8_t>> SlidingCubesBlock::getAllMotions() const {
        auto wrl = getWorld();
        vector<pair<Cell3DPosition,uint8_t>> res;
        vector<SlidingCubesMotionRule *> motions = wrl->getMotionRules()->getValidMotionList(this);

        Cell3DPosition finalPos;
        short finalOrient = 0;
        for (auto &motion:motions) {
            motion->getFinalPositionAndOrientation(this, finalPos, finalOrient);
            res.emplace_back(pair<Cell3DPosition,uint8_t>(finalPos, finalOrient));
        }
        return res;
    }
}
