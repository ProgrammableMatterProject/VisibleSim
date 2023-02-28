/*
 * slidingCubesBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef SLIDINGCUBESBLOCK_H_
#define SLIDINGCUBESBLOCK_H_

#include <stdexcept>

#include "../../gui/openglViewer.h"
#include "../../base/buildingBlock.h"
#include "slidingCubesBlockCode.h"
#include "slidingCubesGlBlock.h"
#include "../../grid/lattice.h"

namespace SlidingCubes {

    const float tabOrientationAngles[6][3] = {{0,      0,      0},
                                              {90.0f,  0.0f,   -90.0f},
                                              {90.0f,  90.0f,  0.0f},
                                              {00.0f,  180.0f, 0.0f},
                                              {90.0f,  0.0f,   90.0f},
                                              {-90.0f, -90.0f, 0.0f}};

    const float tabConnectorPositions[6][3] = { {1,0,0}, {0,1,0}, {0,0,1},
                                                {-1,0,0}, {0,-1,0}, {0,0,-1}};
/*
    const float tabConnectorZ[6][3] = { {0,0,1}, {0,1,0}, {0,0,1},
                                                {-1,0,0}, {0,-1,0}, {0,0,-1}};
*/


    class SlidingCubesBlockCode;

    class SlidingCubesBlock : public BaseSimulator::BuildingBlock {
    protected:
    public:
        SlidingCubesBlock(int bId, BlockCodeBuilder bcb);

        ~SlidingCubesBlock();

        inline SlidingCubesGlBlock *getGlBlock() const override { return (SlidingCubesGlBlock *) ptrGlBlock; };

        inline void setGlBlock(SlidingCubesGlBlock *ptr) { ptrGlBlock = ptr; };

        P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const Cell3DPosition &pos) const;

        inline P2PNetworkInterface *getInterface(SCLattice2::Direction d) const {
            return P2PNetworkInterfaces[d];
        }

        /**
         * @copydoc BuildingBlock::addNeighbor
         */
        virtual void addNeighbor(P2PNetworkInterface *ni, BuildingBlock *target) override;

        /**
         * @copydoc BuildingBlock::removeNeighbor
         */
        virtual void removeNeighbor(P2PNetworkInterface *ni) override;

        int getDirection(P2PNetworkInterface *) const override;

        /**
         * @copydoc BuildingBlock::canMoveTo
         */
        virtual bool canMoveTo(const Cell3DPosition &dest) const override;

        /**
         * @copydoc BuildingBlock::moveTo
         */
        virtual bool moveTo(const Cell3DPosition &dest) override;

        /**
         * @copydoc BuildingBlock::moveTo
         */
        virtual vector<pair<Cell3DPosition,uint8_t>> getAllMotions() const override;

        /**
       @brief Get the orientation code from the transformation matrix of the catom
       @param mat: homogeneous transformation matrix
       @return return orientation code. */
        static uint8_t getOrientationFromMatrix(const Matrix &mat);
        /**
           @brief Get the transformation matrix of the slidingCube from its position in the grid and its orientation code
           @param pos: position of the cell constaining the catom
           @param code: orientation code (number of the connector aligned with x axis)
           @return return homogeneous transformation matrix. */
        static Matrix getMatrixFromPositionAndOrientation(const Cell3DPosition &pos, uint8_t code);

        /**
           @brief Set the slidingCube in the grid according to a cell position and an orientation code
           @param pos: position of the cell constaining the catom
           @param code: orientation code (number of the connector aligned with x axis)*/
        void setPositionAndOrientation(const Cell3DPosition &pos, uint8_t code) override;

/**
       @brief Get the position of the gridcell in the direction of the given connector
       @param connectorId: id of connector (0..11)
       @param pos: position of the cell (if in the grid)
       @return return true if the cell is in the grid, false otherwise. */
        bool getNeighborPos(uint8_t connectorId, Cell3DPosition &pos) const override;

    };

    std::ostream &operator<<(std::ostream &stream, SlidingCubesBlock const &bb);

}

#endif /* SLIDINGCUBESBLOCK_H_ */
