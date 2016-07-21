/*! @file target.cpp
 * @brief Defines a target configuration for reconfiguration algorithms, 
 * several ways of defining the configuration are provided to the user.
 * @author Pierre Thalamy
 * @date 21/07/2016
 */

#include "target.h"
#include "utils.h"

namespace BaseSimulator {

/************************************************************
 *                      TargetGrid
 ************************************************************/

bool TargetGrid::isInTarget(const Cell3DPosition &pos) {
    return tCells.count(pos);
}

const Color TargetGrid::getTargetColor(const Cell3DPosition &pos) {
    if (!isInTarget(pos)) {
        cerr << "error: attempting to get color of undefined target cell" << endl;
        throw new InvalidPositionException();
    }
                
    return tCells[pos];
}

void TargetGrid::addTargetCell(const Cell3DPosition &pos, const Color c) {
    tCells.insert(std::pair<const Cell3DPosition, const Color>(pos, c));
}

/************************************************************
 *                      TargetCSG
 ************************************************************/


bool TargetCSG::isInTarget(const Cell3DPosition &pos) {
    throw new utils::NotImplementedException();
}

const Color TargetCSG::getTargetColor(const Cell3DPosition &pos) {
    throw new utils::NotImplementedException();
}

} // namespace BaseSimulator
