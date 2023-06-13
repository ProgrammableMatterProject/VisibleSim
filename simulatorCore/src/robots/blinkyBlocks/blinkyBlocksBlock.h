/*
 * blinkyBlocksBlock.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef BLINKYBLOCKSBLOCK_H_
#define BLINKYBLOCKSBLOCK_H_

#include <stdexcept>

#include "../../gui/openglViewer.h"
#include "../../base/buildingBlock.h"
#include "../../utils/color.h"
#include "../../grid/lattice.h"
#include "blinkyBlocksBlockCode.h"
#include "blinkyBlocksGlBlock.h"

namespace BlinkyBlocks {

class BlinkyBlocksBlockCode;

class BlinkyBlocksBlock : public BaseSimulator::BuildingBlock {
public:
    BlinkyBlocksBlock(int bId, BlockCodeBuilder bcb);
    ~BlinkyBlocksBlock();

    inline BlinkyBlocksGlBlock* getGlBlock() const override {
        return (BlinkyBlocksGlBlock *)ptrGlBlock;
    };

    P2PNetworkInterface *getInterfaceDestId(int id) const;
    int getDirection(P2PNetworkInterface*) const override;
    Cell3DPosition getRelativePosition(short i) const;
    Cell3DPosition getRelativePosition(P2PNetworkInterface *port) const;
    bool getNeighborPos(uint8_t connectorId,Cell3DPosition &pos) const override;
    P2PNetworkInterface *getInterfaceToNeighborPos(const Cell3DPosition &pos);

    /* schedule the appropriate event for this action */
    /* void tap(Time date); Now a generic event in buildingBlock.cpp */
    void accel(Time date, int x, int y, int z);
    void shake(Time date, int f);

    void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target) override;
    void removeNeighbor(P2PNetworkInterface *ni) override;
    void stopBlock(Time date, State s);
    void pauseClock(Time delay, Time start);

    /**
     * @copydoc BuildingBlock::setPositionAndOrientation
     */
    void setPositionAndOrientation(const Cell3DPosition &p, uint8_t orient) override { setPosition(p); };
    /**
     * @copydoc BuildingBlock::canMoveTo
     */
    bool canMoveTo(const Cell3DPosition& dest) const override { return false; }

    /**
     * @copydoc BuildingBlock::moveTo
     */
    bool moveTo(const Cell3DPosition& dest) override;

    /**
     * @copydoc BuildingBlock::getAllMotions
     */
    virtual vector<pair<Cell3DPosition,uint8_t>> getAllMotions() const override;

};

std::ostream& operator<<(std::ostream &stream, BlinkyBlocksBlock const& bb);

}

#endif /* BLINKYBLOCKSBLOCK_H_ */
