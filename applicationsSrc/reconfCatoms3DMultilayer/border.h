/*
 *  border.h
 *
 *  Created on: 27 November 2017
 *  Author: Thadeu
 */

#ifndef BORDER_H_
#define BORDER_H_

#include "robots/catoms3D/catoms3DWorld.h"
#include "grid/cell3DPosition.h"

class Border {
private:
    static int getNextBorderNeighbor(int &idx, Cell3DPosition &currentPos);
    static int getIdxForBorder(Cell3DPosition);
    static bool isLowestOfBorderOnCurrentPlane(Cell3DPosition);
    static bool isLowestOfBorderOnNextPlane(Cell3DPosition);
    static bool isSeedBorderOnCurrentPlane(Cell3DPosition);
    static bool isSeedBorderOnNextPlane(Cell3DPosition);
    static bool couldBeSeed(Cell3DPosition);
    static Cell3DPosition getCurrentBorderForNextPlane(Cell3DPosition);
    static bool isOnBorder(Cell3DPosition);

public:
    static bool isPlaneSeed(Cell3DPosition);
};

#endif /* BORDER_H_ */
