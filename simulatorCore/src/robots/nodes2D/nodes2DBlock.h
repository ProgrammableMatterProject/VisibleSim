/**
 * @file   nodes2DBlock.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:53:23 2019
 *
 * @brief
 *
 *
 */

#ifndef NODES2DBLOCK_H_
#define NODES2DBLOCK_H_

#include <stdexcept>

#include "base/buildingBlock.h"
#include "robots/nodes2D/nodes2DBlockCode.h"
#include "robots/nodes2D/nodes2DGlBlock.h"
#include "grid/cell3DPosition.h"
#include "grid/lattice.h"
#include "utils/utils.h"

using namespace BaseSimulator::utils;

//! \namespace Nodes2D
namespace Nodes2D {

class Nodes2DBlockCode;

/*! \class Nodes2DBlock
 */
class Nodes2DBlock : public BaseSimulator::BuildingBlock {
public:
    short orientationCode; //!< number of the connector that is along the x axis.
/**
   \brief Constructor
   \param bId: id of the block
   \param bcd : code block function
*/
    Nodes2DBlock(int bId, BlockCodeBuilder bcb);
    ~Nodes2DBlock();

    inline virtual Nodes2DGlBlock* getGlBlock() const override {
        return static_cast<Nodes2DGlBlock*>(ptrGlBlock) ;
    };

    inline void setGlBlock(Nodes2DGlBlock*ptr) { ptrGlBlock=ptr;};

    Cell3DPosition getPosition(SLattice::Direction d) const;
    Cell3DPosition getPosition(P2PNetworkInterface *p2p) const;

/**
   \brief Get the interface from the neighbor position in the grid
   \param pos: position of the cell (if in the grid)
   \return return interface if it exists one connected, NULL otherwise */
    inline P2PNetworkInterface *getInterface(SLattice::Direction d) const { return P2PNetworkInterfaces[d]; }

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
    //bool getNeighborPos(SLattice::Direction connectorDir,Cell3DPosition &pos) const;
    bool getNeighborPos(short connectorId,Cell3DPosition &pos) const override ;


/**
   \brief Get the direction id for the corresponding interface
   \param p2p: pointer to the interface
   \return return value [0..5] of the direction according SLattice::Direction. */
    int getDirection(P2PNetworkInterface*p2p) const override;

    bool hasANeighbor(SLattice::Direction n, bool groundIsNeighbor = false) const;
    bool hasANeighbor(P2PNetworkInterface *p2p, bool groundIsNeighbor = false) const;

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
     * @brief Sets the grid position of the catom, and updates its position matrix
     *
     * @param p :  the grid position (x,y,z) of the block as a Cell3DPosition
     */
    void setPosition(const Cell3DPosition &p) override;
    /**
     *       @brief Get the orientation code from the transformation matrix of the catom
     *       @param mat: homogeneous transformation matrix
     *       @return return orientation code. */
    static short getOrientationFromMatrix(const Matrix &mat);
    /**
     *       @brief Get the transformation matrix of the catom from its position in the grid and its orientation code
     *       @param pos: position of the cell constaining the catom
     *       @param code: orientation code (number of the connector aligned with x axis)
     *       @return return homogeneous transformation matrix. */
    static Matrix getMatrixFromPositionAndOrientation(const Cell3DPosition &pos,short code);
    /**
     *       @brief Set the catom in the grid according to a cell position and an orientation code
     *       @param pos: position of the cell constaining the catom
     *       @param code: orientation code (number of the connector aligned with x axis)*/
    void setPositionAndOrientation(const Cell3DPosition &pos,short code);

    void setDisplayedValue(int n);

    /**
     * @copydoc BuildingBlock::canMoveTo
     */
    virtual bool canMoveTo(const Cell3DPosition& dest) const override;

    /**
     * @copydoc BuildingBlock::moveTo
     */
    virtual bool moveTo(const Cell3DPosition& dest) override;

};

std::ostream& operator<<(std::ostream &stream, Nodes2DBlock const& bb);

}

#endif /* NODES2DBLOCK_H_ */
