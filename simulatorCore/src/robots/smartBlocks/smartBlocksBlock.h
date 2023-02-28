/*
 * smartBlocksBlock.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef SMARTBLOCKSBLOCK_H_
#define SMARTBLOCKSBLOCK_H_

#include "../../gui/openglViewer.h"
#include "../../base/buildingBlock.h"
#include "smartBlocksBlockCode.h"
#include "smartBlocksGlBlock.h"
#include "../../comm/network.h"
#include "../../grid/lattice.h"
#include "../../replay/replayExporter.h"

namespace SmartBlocks {

class SmartBlocksBlockCode;

class SmartBlocksBlock : public BaseSimulator::BuildingBlock {
public:
    SmartBlocksBlock(int bId, BlockCodeBuilder bcb);
    ~SmartBlocksBlock();

    inline P2PNetworkInterface *getInterface(SLattice::Direction d) const {
        return P2PNetworkInterfaces[d];
    }
    P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const Cell3DPosition &pos) const;
    P2PNetworkInterface *getP2PNetworkInterfaceByDestBlockId(bID id) const;

    Cell3DPosition getPosition(SLattice::Direction d) const;
    int getDirection( P2PNetworkInterface*) const override;
    inline void getGridPosition(int &x,int &y) const {
        x = int(position[0]); y=int(position[1]);
    };

    /**
     * @copydoc BuildingBlock::addNeighbor
     */
    void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) override;
    /**
     * @copydoc BuildingBlock::removeNeighbor
     */
    void removeNeighbor(P2PNetworkInterface *ni) override;

    void setDisplayedValue(uint16_t n);
    void disableDisplay() { setDisplayedValue(SmartBlocksGlBlock::noDisplay); }

    void setPositionAndOrientation(const Cell3DPosition &p, uint8_t orient) { setPosition(p); };
    /**
     * @param dest
     * @return true if the module can move to position dest
     *         (it is in the grid, adjacent and free)
     */
    bool canMoveTo(const Cell3DPosition& dest) const override;

    /**
     * @brief Moves the current module to dest if possible (it is in the grid, adjacent and free)
     * @param dest
     * @return true module has initiated a motion to dest, false if it is not possible
     */
    bool moveTo(const Cell3DPosition& dest) override;

    /**
    * @copydoc BuildingBlock::getAllMotions
    */
    virtual vector<pair<Cell3DPosition,uint8_t>> getAllMotions() const override;

    /**
     * Serializes (converts to a stream of bits) relevant data from the building block object
     *  for the purpose of simulation replay
     *
     *  By default, serializes as: <id><position><orientation><color>
     *  Extra attributes can be serialized in children classes
     *
     * @param bStream output binary stream
     */
    void serialize(std::ofstream &bStream) override;

    /**
     * Clear-text equivalent of the BuildingBlock::serialize function, for debugging purpose
     * @see BuildingBlock::serialize
     * @param dbStream output binary stream
     */
    void serialize_cleartext(std::ofstream &dbStream) override;

};

}

#endif /* SMARTBLOCKSBLOCK_H_ */
