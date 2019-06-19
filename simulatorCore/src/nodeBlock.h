/**
 * @file   nodeBlock.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 13:53:23 2019
 *
 * @brief
 *
 *
 */

#ifndef NODEBLOCK_H_
#define NODEBLOCK_H_

#include <stdexcept>

#include "buildingBlock.h"
#include "nodeBlockCode.h"
#include "nodeGlBlock.h"
#include "cell3DPosition.h"
#include "lattice.h"
#include "utils.h"

using namespace BaseSimulator::utils;

//! \namespace Node
namespace Node {

class NodeBlockCode;

/*! \class NodeBlock
*/
class NodeBlock : public BaseSimulator::BuildingBlock {
    short orientationCode; //!< number of the connector that is along the x axis.
public:
/**
   \brief Constructor
   \param bId: id of the block
   \param bcd : code block function
*/
    NodeBlock(int bId, BlockCodeBuilder bcb);
    ~NodeBlock();

    inline virtual NodeGlBlock* getGlBlock() const override {
        return static_cast<NodeGlBlock*>(ptrGlBlock) ;
    };

    inline void setGlBlock(NodeGlBlock*ptr) { ptrGlBlock=ptr;};

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
    bool getNeighborPos(SLattice::Direction connectorDir,Cell3DPosition &pos) const;

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

};

std::ostream& operator<<(std::ostream &stream, NodeBlock const& bb);

}

#endif /* NODEBLOCK_H_ */
