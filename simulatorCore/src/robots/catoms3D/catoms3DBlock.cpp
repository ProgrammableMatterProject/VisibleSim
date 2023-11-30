/*!
 * \file catoms3DBlock.cpp
 * \brief catoms Block
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#include <iostream>
#include "../../base/buildingBlock.h"
#include "../../utils/trace.h"
#include "../../replay/replayExporter.h"
#include "catoms3DBlock.h"
#include "catoms3DWorld.h"
#include "catoms3DSimulator.h"
#include "catoms3DMotionEngine.h"

using namespace std;

namespace Catoms3D {

    Catoms3DBlock::Catoms3DBlock(int bId, BlockCodeBuilder bcb)
            : BaseSimulator::BuildingBlock(bId, bcb, FCCLattice::MAX_NB_NEIGHBORS) {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << "Catoms3DBlock constructor" << endl;
#endif

        orientationCode = 0; // connector 0 is along X axis
    }

    Catoms3DBlock::~Catoms3DBlock() {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << "Catoms3DBlock destructor " << blockId << endl;
#endif
    }

    void Catoms3DBlock::setVisible(bool visible) {
        getWorld()->updateGlData(this, visible);
    }

    Matrix Catoms3DBlock::getMatrixFromPositionAndOrientation(const Cell3DPosition &pos,
                                                              uint8_t code) {
        uint8_t orientation = code % 12;
        uint8_t up = code / 12;

        Matrix M1, M2, M3, M;
        M1.setRotationZ(tabOrientationAngles[orientation][2]);
        M2.setRotationY(tabOrientationAngles[orientation][1]);
        M3.setRotationX(tabOrientationAngles[orientation][0] + up * 180.0);
        M = M2 * M1;
        M1 = M3 * M;
        M2.setTranslation(getWorld()->lattice->gridToWorldPosition(pos));
        M = M2 * M1;
        return M;
    }

    void Catoms3DBlock::setPosition(const Cell3DPosition &p) {
        setPositionAndOrientation(p, orientationCode);
        if (ReplayExporter::isReplayEnabled())
            ReplayExporter::getInstance()->writePositionUpdate(getScheduler()->now(),
                                                               blockId, position, orientationCode);
    }

    void Catoms3DBlock::setPositionAndOrientation(const Cell3DPosition &pos, uint8_t code) {
        orientationCode = code;
        position = pos;

        Matrix M = getMatrixFromPositionAndOrientation(pos, code);
        getWorld()->updateGlData(this, M);
        getWorld()->updateGlData(this, position);
        if (ReplayExporter::isReplayEnabled())
            ReplayExporter::getInstance()->writePositionUpdate(getScheduler()->now(),
                                                               blockId, position, orientationCode);

    }

    uint8_t Catoms3DBlock::getOrientationFromMatrix(const Matrix &mat) {
        Vector3D x(1.0, 0.0, 0.0, 0.0); // Vector3D X
        Vector3D v;
        //p = mat*x;
        /*Matrix mat_1;
        mat.inverse(mat_1);*/

        uint8_t current = -1;
        double psmax = -1;
        for (int i = 0; i < 12; i++) {
            x.set(tabConnectorPositions[i], 3);
            v = mat * x;
            //OUTPUT << "connector #" << i << ":" << v << endl;
            if (v[0] > psmax) {
                current = i;
                psmax = v[0];
            }
        }
        // orientation autour du connecteur
        Matrix M1, M2, M3, M;
        M1.setRotationZ(tabOrientationAngles[current][2]);
        M2.setRotationY(tabOrientationAngles[current][1]);
        M3.setRotationX(tabOrientationAngles[current][0]);
        M = M2 * M1;
        M1 = M3 * M;
        M1.inverse(M);
        M.m[15] = 0;
        /*OUTPUT << "----- ref -----" << endl;
          OUTPUT << M << endl;
          OUTPUT << "----- mat -----" << endl;*/
        //M3 = mat;
        //OUTPUT << M3 << endl;

        M2 = mat * M;
        //OUTPUT << M2 << endl;
        // detection of a rotation matrix PI around X axis if M2.m[10]=env.-1
        if (M2.m[10] < 0) {
            current = current + 12;
        }

        //OUTPUT << "result =" << current << endl;
        return current;
    }

    int Catoms3DBlock::getDirection(P2PNetworkInterface *given_interface) const {
        if (!given_interface) {
            return -1;
        }
        for (int i(0); i < 12; ++i) {
            if (P2PNetworkInterfaces[i] == given_interface) return i;
        }
        return -1;
    }

    uint8_t Catoms3DBlock::getAbsoluteDirection(uint8_t connector) const {
        Cell3DPosition conPos; // cell adjacent to connector
        bool posIsValid = getNeighborPos(connector, conPos);

        if (!posIsValid) return -1;
        Lattice *lattice = Catoms3DWorld::getWorld()->lattice;
        return lattice->getDirection(position, conPos);
    }

    uint8_t Catoms3DBlock::getAbsoluteDirection(const Cell3DPosition &pos) const {
        return getAbsoluteDirection(getConnectorId(pos));
    }

    uint8_t Catoms3DBlock::projectAbsoluteNeighborDirection(const Cell3DPosition &nPos,
                                                            uint8_t nDirection) const {
        // cout << "pAND: " << "nPos: " << nPos << "/" << nDirection << endl
        //      << "\tPosition: " << position << endl;

        // Find cell on direction nDirection of neighbor at nPos
        Lattice *lattice = Catoms3DWorld::getWorld()->lattice;
        Cell3DPosition projectedPos = lattice->getCellInDirection(nPos, nDirection);
        // cout << "\tproj: " << projectedPos << endl;

        // No corresponding connector on current module
        if (!lattice->cellsAreAdjacent(position, projectedPos)) return -1;

        // Find connector adjacent to projectedPos on current module
        return getConnectorId(projectedPos);
    }

    bool Catoms3DBlock::getNeighborPos(uint8_t connectorID, Cell3DPosition &pos) const {
        Vector3D realPos;

        Catoms3DWorld *wrl = getWorld();
        const Vector3D bs = wrl->lattice->gridScale;

        realPos.set(tabConnectorPositions[connectorID], 3, 1);
        realPos *= bs;
        realPos.set(3, 1.0); // A vérifier
        realPos = ((Catoms3DGlBlock *) ptrGlBlock)->mat * realPos;
        if (realPos[2] < 0) return false;
        pos = wrl->lattice->worldToGridPosition(realPos);
        return wrl->lattice->isInGrid(pos);
    }

    P2PNetworkInterface *Catoms3DBlock::getInterface(const Cell3DPosition &pos) const {
        Catoms3DWorld *wrl = getWorld();
        if (not wrl->lattice->cellsAreAdjacent(position, pos)) return nullptr;
        uint8_t conId = getConnectorId(pos);

        return conId >= 0 ? P2PNetworkInterfaces[conId] : nullptr;
    }

    uint8_t Catoms3DBlock::getConnectorId(const Cell3DPosition &pos) const {
        Catoms3DWorld *wrl = getWorld();

        if (!wrl->lattice->isInGrid(pos))
            return -1;

        Vector3D realPos = wrl->lattice->gridToWorldPosition(pos);

        Matrix m_1;
        ((Catoms3DGlBlock *) ptrGlBlock)->mat.inverse(m_1);
        realPos = m_1 * realPos;

        const Vector3D bs = wrl->lattice->gridScale;
        /*realPos.pt[0] /= bs[0];
        realPos.pt[1] /= bs[1];
        realPos.pt[2] /= bs[2];*/
        realPos /= bs;

        double x, y, z, d = 1;
        int i = 0;

        while (i < 12 && d > 0.1) {
            x = tabConnectorPositions[i][0] - realPos[0];
            y = tabConnectorPositions[i][1] - realPos[1];
            z = tabConnectorPositions[i][2] - realPos[2];
            d = x * x + y * y + z * z;
            i++;
        }

        return d > 0.1 ? -1 : i - 1;
    }

    Catoms3DBlock *Catoms3DBlock::getNeighborOnCell(const Cell3DPosition &pos) const {
        Lattice *lattice = getWorld()->lattice;

        if (!lattice->cellsAreAdjacent(position, pos)) return nullptr;

        return static_cast<Catoms3DBlock *>(lattice->getBlock(pos));
    }

    bool Catoms3DBlock::areOrientationsInverted(uint8_t otherOriCode) const {
        return ((orientationCode / 12) + (otherOriCode / 12)) == 1;
    }

    void Catoms3DBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock *target) {
#ifdef DEBUG_NEIGHBORHOOD
        OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
               << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
        getScheduler()->schedule(
                new AddNeighborEvent(getScheduler()->now(), this,
                                     getWorld()->lattice->getOppositeDirection(getDirection(ni)), target->blockId));
    }

    void Catoms3DBlock::removeNeighbor(P2PNetworkInterface *ni) {
#ifdef DEBUG_NEIGHBORHOOD
        OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
               << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
        getScheduler()->schedule(
                new RemoveNeighborEvent(getScheduler()->now(), this,
                                        getWorld()->lattice->getOppositeDirection(getDirection(ni))));
    }

    Catoms3DBlock *Catoms3DBlock::getNeighborBlock(const Cell3DPosition &nPos) const {
        return static_cast<Catoms3DBlock *>(getWorld()->lattice->getBlock(nPos));
    }

    Catoms3DBlock *Catoms3DBlock::getNeighborBlock(uint8_t conId) const {
        P2PNetworkInterface *itfId = getInterface(conId);

        if (itfId and itfId->isConnected())
            return static_cast<Catoms3DBlock *>(itfId->connectedInterface->hostBlock);

        return nullptr;
    }

    bool Catoms3DBlock::canRotateToPosition(const Cell3DPosition &pos,
                                            RotationLinkType faceReq) const {
        return Catoms3DMotionEngine::findMotionPivot(this, pos, faceReq) != nullptr;
    }

    std::bitset<12> Catoms3DBlock::getLocalNeighborhoodState() const {
        bitset<12> bitset = {0};
        const vector<Cell3DPosition> localNeighborhood =
                Catoms3DWorld::getWorld()->lattice->getNeighborhood(position);

        for (const Cell3DPosition &nPos: localNeighborhood) {
            P2PNetworkInterface *itf = getInterface(nPos);
            bitset.set(getAbsoluteDirection(nPos), itf->isConnected());
        }

        return bitset;
    }

    void Catoms3DBlock::exportMatrix() const {
        OUTPUT << getScheduler()->now() << "|";
        OUTPUT << blockId << "|";
        OUTPUT << color << "|";
        const Matrix &m = getMatrixFromPositionAndOrientation(position, orientationCode);
        OUTPUT << "(matrix3 "
               << "[" << m.m[0] << "," << m.m[4] << "," << m.m[8] << "] "
               << "[" << m.m[1] << "," << m.m[5] << "," << m.m[9] << "] "
               << "[" << m.m[2] << "," << m.m[6] << "," << m.m[10] << "] "
               << "[" << m.m[3] << "," << m.m[7] << "," << m.m[11] << "])"
               << endl;
    }

    bool Catoms3DBlock::canMoveTo(const Cell3DPosition &dest) const {
        return canRotateToPosition(dest);
    }

    bool Catoms3DBlock::moveTo(const Cell3DPosition &dest) {
        if (not canMoveTo(dest))
            return false;

        getScheduler()->schedule(
                new Catoms3DRotationStartEvent(getScheduler()->now(), this, dest));

        return true;
    }

    vector<pair<Cell3DPosition, uint8_t>> Catoms3DBlock::getAllMotions() const {
        vector<pair<Cell3DPosition, uint8_t>> res;
        vector<std::pair<const Catoms3DMotionRulesLink *, Catoms3DRotation>> tab = Catoms3DMotionEngine::getAllRotationsForModule(
                this);
        if (tab.empty()) return res;
        Cell3DPosition finalPos;
        short finalOrient;
        auto lattice = Catoms3DWorld::getWorld()->lattice;
        for (auto &elem: tab) {
            elem.second.init(((Catoms3DGlBlock *) ptrGlBlock)->mat);
            elem.second.getFinalPositionAndOrientation(finalPos, finalOrient);
            cout << "Xpos:" << finalPos << endl;
            if (lattice->isInGrid(finalPos) && lattice->isFree(finalPos)) {
                res.emplace_back(pair<Cell3DPosition, uint8_t>(finalPos, finalOrient));
            }
        }
        return res;
    }
}