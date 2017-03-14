#include "neighborRestriction.h"
#include "catoms3DWorld.h"

using namespace Catoms3D;

int NeighborRestriction::neighborDirectionsEven[12][3] = {{0,0,-1},{-1,-1,-1},{-1,0,-1},{0,-1,-1},{0,0,1},{-1,-1,1},{-1,0,1},{0,-1,1},{1,0,0},{0,1,0},{-1,0,0},{0,-1,0}};
int NeighborRestriction::neighborDirectionsOdd[12][3] = {{0,0,-1},{1,1,-1},{1,0,-1},{0,1,-1},{0,0,1},{1,1,1},{1,0,1},{0,1,1},{1,0,0},{0,1,0},{-1,0,0},{0,-1,0}};

int sideOneOddXY[4][3] = {{1,0,-1},{1,1,-1},{0,1,-1},{0,0,-1}};
int sideTwoOddXY[4][3] = {{1,0,1},{1,1,1},{0,1,1},{0,0,1}};
int sideOneEvenXY[4][3] = {{0,0,-1},{-1,-1,-1},{-1,0,-1},{0,-1,-1}};
int sideTwoEvenXY[4][3] = {{0,0,1},{-1,-1,1},{-1,0,1},{0,-1,1}};

int sideOneOddXZ[5][3] = {{1,0,-1},{1,1,-1},{1,1,1},{1,0,1},{1,0,0}};
int sideTwoOddXZ[5][3] = {{0,1,-1},{0,0,-1},{0,1,1},{0,0,1},{-1,0,0}};
int sideOneEvenXZ[5][3] = {{0,0,-1},{0,-1,-1},{0,0,1},{0,-1,1},{1,0,0}};
int sideTwoEvenXZ[5][3] = {{-1,0,-1},{-1,-1,-1},{-1,0,1},{-1,-1,1},{-1,0,0}};

int sideOneOddYZ[5][3] = {{0,0,-1},{1,0,-1},{0,0,1},{1,0,1},{0,-1,0}};
int sideTwoOddYZ[5][3] = {{1,1,-1},{0,1,-1},{1,1,1},{0,1,1},{0,1,0}};
int sideOneEvenYZ[5][3] = {{0,-1,-1},{-1,-1,-1},{0,-1,1},{-1,-1,1},{0,-1,0}};
int sideTwoEvenYZ[5][3] = {{0,0,-1},{-1,0,-1},{0,0,1},{-1,0,1},{0,1,0}};

bool NeighborRestriction::isPositionUnblockedSide(const Cell3DPosition &pos) {
    int xyPos[4][3] = {{-1,0,0}, {1,0,0}, {0,-1,0}, {0,1,0}};
    Cell3DPosition occupiedPosition(pos[0]+xyPos[0][0],pos[1]+xyPos[0][1], pos[2]+xyPos[0][2]);
    Cell3DPosition forbiddenPosition(pos[0]+xyPos[1][0], pos[1]+xyPos[1][1], pos[2]+xyPos[1][2]);
    if (cellHasBlock(occupiedPosition) && cellHasBlock(forbiddenPosition))
        return false;
    occupiedPosition.set(pos[0]+xyPos[2][0],pos[1]+xyPos[2][1], pos[2]+xyPos[2][2]);
    forbiddenPosition.set(pos[0]+xyPos[3][0], pos[1]+xyPos[3][1], pos[2]+xyPos[3][2]);
    if (cellHasBlock(occupiedPosition) && cellHasBlock(forbiddenPosition))
        return false;
    return true;
}

bool NeighborRestriction::isPositionUnblockedXY(const Cell3DPosition &pos) {
    int *sideOne, *sideTwo;
    Cell3DPosition position1, position2;
    bool isInSide1 = false, isInSide2 = false;
    for (int i = 0; i < 4; i++) {
        if (pos[2]%2) {
            sideOne = sideOneOddXY[i];
            sideTwo = sideTwoOddXY[i];
        }
        else {
            sideOne = sideOneEvenXY[i];
            sideTwo = sideTwoEvenXY[i];
        }
        position1.set(pos[0]+sideOne[0], pos[1] + sideOne[1], pos[2] + sideOne[2]); 
        position2.set(pos[0]+sideTwo[0], pos[1] + sideTwo[1], pos[2] + sideTwo[2]); 
        if (cellHasBlock(position1))
            isInSide1 = true;
        if (cellHasBlock(position2))
            isInSide2 = true;
    }
    if (isInSide1 && isInSide2)
        return false;
    return true;
}

bool NeighborRestriction::isPositionUnblockedYZ(const Cell3DPosition &pos) {
    int *sideOne, *sideTwo;
    Cell3DPosition position1, position2;
    bool isInSide1 = false, isInSide2 = false;
    for (int i = 0; i < 5; i++) {
        if (pos[2]%2) {
            sideOne = sideOneOddYZ[i];
            sideTwo = sideTwoOddYZ[i];
        }
        else {
            sideOne = sideOneEvenYZ[i];
            sideTwo = sideTwoEvenYZ[i];
        }
        position1.set(pos[0]+sideOne[0], pos[1] + sideOne[1], pos[2] + sideOne[2]); 
        position2.set(pos[0]+sideTwo[0], pos[1] + sideTwo[1], pos[2] + sideTwo[2]); 
        if (cellHasBlock(position1))
            isInSide1 = true;
        if (cellHasBlock(position2))
            isInSide2 = true;
    }
    if (isInSide1 && isInSide2)
        return false;
    return true;
}

bool NeighborRestriction::isPositionUnblockedXZ(const Cell3DPosition &pos) {
    int *sideOne, *sideTwo;
    Cell3DPosition position1, position2;
    bool isInSide1 = false, isInSide2 = false;
    for (int i = 0; i < 5; i++) {
        if (pos[2]%2) {
            sideOne = sideOneOddXZ[i];
            sideTwo = sideTwoOddXZ[i];
        }
        else {
            sideOne = sideOneEvenXZ[i];
            sideTwo = sideTwoEvenXZ[i];
        }
        position1.set(pos[0]+sideOne[0], pos[1] + sideOne[1], pos[2] + sideOne[2]); 
        position2.set(pos[0]+sideTwo[0], pos[1] + sideTwo[1], pos[2] + sideTwo[2]); 
        if (cellHasBlock(position1))
            isInSide1 = true;
        if (cellHasBlock(position2))
            isInSide2 = true;
    }
    if (isInSide1 && isInSide2)
        return false;
    return true;
}

bool NeighborRestriction::isPositionBlocked(const Cell3DPosition &pos) {
    if (isPositionUnblockedSide(pos)) {
        if (isPositionUnblockedXZ(pos) || isPositionUnblockedYZ(pos) || isPositionUnblockedXY(pos)) {
            return false;
        }
    }
    return true;
}

bool NeighborRestriction::isPositionBlockable(const Cell3DPosition &pos) {
    Cell3DPosition neighborPos;
    Catoms3DWorld *world = Catoms3DWorld::getWorld();
    int *direction;
    for (int i = 0; i < 12; i++) {
        direction = pos[2]%2 ? neighborDirectionsOdd[i] : neighborDirectionsEven[i];
        neighborPos.set(pos[0]+direction[0], pos[1] + direction[1], pos[2] + direction[2]); 
        if (world->lattice->isFree(neighborPos) && !isPositionBlocked(neighborPos)) {
           simulatedBlockPosition = pos; 
           if (isPositionBlocked(neighborPos))
               return true;
           simulatedBlockPosition.set(0,0,0);
        }
    }
    return false;
}

bool NeighborRestriction::cellHasBlock(const Cell3DPosition &pos) {
    if (simulatedBlockPosition == pos)
        return true;
    return Catoms3DWorld::getWorld()->lattice->cellHasBlock(pos);
}
