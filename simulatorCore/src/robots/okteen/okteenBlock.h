/*!
 * \file okteenBlock.h
 * \brief okteen Block
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#ifndef OKTEENBLOCK_H_
#define OKTEENBLOCK_H_

#include <stdexcept>

#include "base/buildingBlock.h"
#include "robots/okteen/okteenBlockCode.h"
#include "robots/okteen/okteenGlBlock.h"
#include "grid/cell3DPosition.h"
#include "grid/lattice.h"
#include "utils/utils.h"

using namespace BaseSimulator::utils;

//! \namespace Okteen
namespace Okteen {

class OkteenBlockCode;

/*! \class OkteenBlock
    \brief Special treatement and data for 3D quasi-spherical robot
*/
class OkteenBlock : public BaseSimulator::BuildingBlock {
public :
    short orientationCode; //!< number of the connector that is along the x axis.
public:
/**
   \brief Constructor
   \param bId: id of the block
   \param bcd : code block function
*/
    OkteenBlock(int bId, BlockCodeBuilder bcb);
    ~OkteenBlock();

    inline virtual OkteenGlBlock* getGlBlock() const override {
        return static_cast<OkteenGlBlock*>(ptrGlBlock) ;
    };

    inline void setGlBlock(OkteenGlBlock*ptr) { ptrGlBlock=ptr;};
/**
   \brief Get the interface from the neighbor position in the grid
   \param pos: position of the cell (if in the grid)
   \return return interface if it exists one connected, NULL otherwise */
    inline P2PNetworkInterface *getInterface(SCLattice::Direction d) const { return P2PNetworkInterfaces[d]; }
/**
   \brief Get the interface from the interface id
   \param id: interface number
   \return return interface if it exists one connected, NULL otherwise */
    inline P2PNetworkInterface *getInterface(int id) const { return P2PNetworkInterfaces[id]; };
/**
   \brief Get the position of the gridcell in the direction of the given connector
   \param connectorDir: direction code of the neighbor (Bottom=0, Back=1, Right=2, Left=3, Front=4, Top=5)
   \param pos: position of the cell (if in the grid)
   \return return true if the cell is in the grid, false otherwise. */
    bool getNeighborPos(SCLattice::Direction connectorDir,Cell3DPosition &pos) const;
    using BuildingBlock::getNeighborPos; // Suppresses hiding warning
/**
   \brief Get the direction id for the corresponding interface
   \param p2p: pointer to the interface
   \return return value [0..5] of the direction according SCLattice::Direction. */
    int getDirection(P2PNetworkInterface*p2p) const override;
/**
   \brief Set the length of connector
   \param connectorId: id of connector (0..5)
   \param length: between 0 (closed) and 1.0 (activated). */
    void setConnectorLength(short connectorId,float length);
/**
   \brief Get the orientation code from the transformation matrix of the catom
   \param mat: homogeneous transformation matrix
   \return return orientation code.
    static short getOrientationFromMatrix(const Matrix &mat);*/
/**
   \brief Get the transformation matrix of the catom from its position in the grid and its orientation code
   \param pos: position of the cell constaining the catom
   \param code: orientation code (number of the connector aligned with x axis)
   \return return homogeneous transformation matrix.
    static Matrix getMatrixFromPositionAndOrientation(const Cell3DPosition &pos,short code);*/
/**
   \brief Set the catom in the grid according to a cell position and an orientation code
   \param pos: position of the cell constaining the catom
   \param code: orientation code (number of the connector aligned with x axis)
    void setPositionAndOrientation(const Cell3DPosition &pos,short code);*/

    // MeldInterpreter
    /**
     * @copydoc BuildingBlock::addNeighbor
     */
    virtual void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) override;
    /**
     * @copydoc BuildingBlock::removeNeighbor
     */
    virtual void removeNeighbor(P2PNetworkInterface *ni) override;

    /**
     * @copydoc BuildingBlock::canMoveTo
     */
    virtual bool canMoveTo(const Cell3DPosition& dest) const override;

    /**
     * @copydoc BuildingBlock::moveTo
     */
    virtual bool moveTo(const Cell3DPosition& dest) override;
};

std::ostream& operator<<(std::ostream &stream, OkteenBlock const& bb);

}

#endif /* OKTEENBLOCK_H_ */
