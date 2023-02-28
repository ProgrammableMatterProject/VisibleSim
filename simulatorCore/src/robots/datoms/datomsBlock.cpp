/*!
 * \file datomsBlock.cpp
 * \brief deformable atoms Block
 * \date 28/01/2018
 * \author Benoît Piranda
 */

#include <iostream>
#include "datomsBlock.h"
#include "../../base/buildingBlock.h"
#include "datomsWorld.h"
#include "datomsSimulator.h"
#include "../../utils/trace.h"
#include "../../replay/replayExporter.h"

using namespace std;

//! \namespace Datoms
namespace Datoms {

    DatomsBlock::DatomsBlock(int bId, BlockCodeBuilder bcb)
            : BaseSimulator::BuildingBlock(bId, bcb, FCCLattice::MAX_NB_NEIGHBORS) {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << "DatomsBlock constructor" << endl;
#endif

        orientationCode = 0; // connector 0 is along X axis
    }

    DatomsBlock::~DatomsBlock() {
#ifdef DEBUG_OBJECT_LIFECYCLE
        OUTPUT << "DatomsBlock destructor " << blockId << endl;
#endif
    }

    void DatomsBlock::setVisible(bool visible) {
        getWorld()->updateGlData(this, visible);
    }

    Matrix DatomsBlock::getMatrixFromPositionAndOrientation(const Cell3DPosition &pos, uint8_t code) {
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

    void DatomsBlock::setPosition(const Cell3DPosition &p) {
        setPositionAndOrientation(p, orientationCode);
    }

    void DatomsBlock::setPositionAndOrientation(const Cell3DPosition &pos, uint8_t code) {
        orientationCode = code;
        position = pos;

        Matrix M = getMatrixFromPositionAndOrientation(pos, code);
        getWorld()->updateGlData(this, M);
        getWorld()->updateGlData(this, position); // necessary for picking

        if (ReplayExporter::isReplayEnabled())
            ReplayExporter::getInstance()->writePositionUpdate(getScheduler()->now(),
                                                               blockId, position, orientationCode);
    }

    uint8_t DatomsBlock::getOrientationFromMatrix(const Matrix &mat) {
        Vector3D x(1.0, 0.0, 0.0, 0.0); // Vector3D X
        Vector3D v;
        //p = mat*x;
        Matrix mat_1;
        mat.inverse(mat_1);

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
        M3 = mat;
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

    int DatomsBlock::getDirection(P2PNetworkInterface *given_interface) const {
        if (!given_interface) {
            return -1;
        }
        for (int i(0); i < 12; ++i) {
            if (P2PNetworkInterfaces[i] == given_interface) return i;
        }
        return -1;
    }

    uint8_t DatomsBlock::getAbsoluteDirection(uint8_t connector) const {
        Cell3DPosition conPos; // cell adjacent to connector
        bool posIsValid = getNeighborPos(connector, conPos);

        if (!posIsValid) return -1;
        Lattice *lattice = DatomsWorld::getWorld()->lattice;
        return lattice->getDirection(position, conPos);
    }

    uint8_t DatomsBlock::projectAbsoluteNeighborDirection(const Cell3DPosition &nPos, uint8_t nDirection) const {
        // cout << "pAND: " << "nPos: " << nPos << "/" << nDirection << endl
        //      << "\tPosition: " << position << endl;

        // Find cell on direction nDirection of neighbor at nPos
        Lattice *lattice = DatomsWorld::getWorld()->lattice;
        Cell3DPosition projectedPos = lattice->getCellInDirection(nPos, nDirection);
        // cout << "\tproj: " << projectedPos << endl;

        // No corresponding connector on current module
        if (!lattice->cellsAreAdjacent(position, projectedPos)) return -1;

        // Find connector adjacent to projectedPos on current module
        return getConnectorId(projectedPos);
    }


    std::ostream &operator<<(std::ostream &stream, DatomsBlock const &bb) {
        stream << bb.blockId << "\tcolor: " << bb.color;
        return stream;
    }

    bool DatomsBlock::getNeighborPos(uint8_t connectorID, Cell3DPosition &pos) const {
        Vector3D realPos;

        DatomsWorld *wrl = getWorld();
        const Vector3D bs = wrl->lattice->gridScale;

        realPos.set(tabConnectorPositions[connectorID], 3, 1);
        /*realPos.pt[0] *= bs[0];
        realPos.pt[1] *= bs[1];
        realPos.pt[2] *= bs[2];*/
        realPos *= bs;
        realPos.set(3, 1.0); // to check
        realPos = ((DatomsGlBlock *) ptrGlBlock)->mat * realPos;
        if (realPos[2] < 0) return false;
        pos = wrl->lattice->worldToGridPosition(realPos);
        return wrl->lattice->isInGrid(pos);
    }

    P2PNetworkInterface *DatomsBlock::getInterface(const Cell3DPosition &pos) const {
        uint8_t conId = getConnectorId(pos);

        return conId >= 0 ? P2PNetworkInterfaces[conId] : NULL;
    }

    uint8_t DatomsBlock::getConnectorId(const Cell3DPosition &pos) const {
        DatomsWorld *wrl = getWorld();

        if (!wrl->lattice->isInGrid(pos))
            return -1;

        Vector3D realPos = wrl->lattice->gridToWorldPosition(pos);

        Matrix m_1;
        ((DatomsGlBlock *) ptrGlBlock)->mat.inverse(m_1);
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

    DatomsBlock *DatomsBlock::getNeighborOnCell(const Cell3DPosition &pos) const {
        Lattice *lattice = getWorld()->lattice;

        if (!lattice->cellsAreAdjacent(position, pos)) return NULL;

        return static_cast<DatomsBlock *>(lattice->getBlock(pos));
    }

    bool DatomsBlock::areOrientationsInverted(uint8_t otherOriCode) const {
        return ((orientationCode / 12) + (otherOriCode / 12)) == 1;
    }

    void DatomsBlock::addNeighbor(P2PNetworkInterface *ni, BuildingBlock *target) {
#ifdef DEBUG_NEIGHBORHOOD
        OUTPUT << "Simulator: "<< blockId << " add neighbor " << target->blockId << " on "
               << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
        getScheduler()->schedule(
                new AddNeighborEvent(getScheduler()->now(), this,
                                     getWorld()->lattice->getOppositeDirection(getDirection(ni)), target->blockId));
    }

    void DatomsBlock::removeNeighbor(P2PNetworkInterface *ni) {
#ifdef DEBUG_NEIGHBORHOOD
        OUTPUT << "Simulator: "<< blockId << " remove neighbor on "
               << getWorld()->lattice->getDirectionString(getDirection(ni)) << endl;
#endif
        getScheduler()->schedule(
                new RemoveNeighborEvent(getScheduler()->now(), this,
                                        getWorld()->lattice->getOppositeDirection(getDirection(ni))));
    }

// \todo Implement function
// bool DatomsBlock::canRotateToPosition(const Cell3DPosition &pos) const {
//     return Catoms3DMotionEngine::findMotionPivot(this, pos, faceReq) != NULL;
// }

    bool DatomsBlock::canMoveTo(const Cell3DPosition &dest) const {
        throw NotImplementedException("canMoveTo not implemented yet");
    }

    bool DatomsBlock::moveTo(const Cell3DPosition &dest) {
        throw NotImplementedException("moveTo not implemented yet");
    }

    vector<pair<Cell3DPosition, uint8_t>> DatomsBlock::getAllMotions() const {
        vector<pair<Cell3DPosition, uint8_t>> res;
        throw NotImplementedException("moveTo not implemented yet");
        return res;
    }

}
