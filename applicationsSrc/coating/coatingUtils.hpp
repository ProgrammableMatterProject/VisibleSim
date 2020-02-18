/**
 * @file   coatingUtils.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Fri Oct 11 11:13:32 2019
 *
 * @brief
 *
 *
 */

#ifndef COATING_UTILS__H_
#define COATING_UTILS__H_

//!< CounterClockwise Directions for navigating around a structure
enum PlanarDir {NorthWest, North, NorthEast, East, SouthEast, South, SouthWest, West };

inline const int NumPlanarDirs = 8;
inline constexpr Cell3DPosition planarPos[NumPlanarDirs] = {
    Cell3DPosition(-1, 1, 0), // FrontLeft
    Cell3DPosition(0, 1, 0), // Front
    Cell3DPosition(1, 1, 0), // FrontRight
    Cell3DPosition(1, 0, 0), // Right
    Cell3DPosition(1,-1, 0), // RearRight
    Cell3DPosition(0, -1, 0), // Rear
    Cell3DPosition(-1, -1, 0), // RearLeft
    Cell3DPosition(-1, 0, 0), // Left
};

inline int getDirectionIndexForPosition(const Cell3DPosition& pos) {
    for (int i = 0; i < NumPlanarDirs; i++) {
        if (planarPos[i] == pos) return i;
    }

    return -1;
}

inline string planarDirectionIndexToString(PlanarDir d) {
    switch(d) {
        case NorthWest: return "NorthWest";
        case North: return "North";
        case NorthEast: return "NorthEast";
        case East: return "East";
        case SouthEast: return "SouthEast";
        case South: return "South";
        case SouthWest: return "SouthWest";
        case West: return "West";
        default: return "#Error";
    }

    return ""; // Unreachable
}

inline string planarDirectionPositionToString(const Cell3DPosition& pos) {
    switch(getDirectionIndexForPosition(pos)) {
        case 0: return "NorthWest";
        case 1: return "North";
        case 2: return "NorthEast";
        case 3: return "East";
        case 4: return "SouthEast";
        case 5: return "South";
        case 6: return "SouthWest";
        case 7: return "West";
        default: return "#Error";
    }

    return ""; // Unreachable
}

inline constexpr Cell3DPosition horizontalNeighbors[4] = { Cell3DPosition(0,1,0),
    Cell3DPosition(1,0,0), Cell3DPosition(0,-1,0), Cell3DPosition(-1,0,0), };

inline constexpr Cell3DPosition diagNeighbors[4] = { Cell3DPosition(-1,-1,0),
    Cell3DPosition(1,-1,0), Cell3DPosition(-1,1,0), Cell3DPosition(1,1,0), };

inline constexpr Cell3DPosition diag2Neighbors[4] = { Cell3DPosition(-2,-2,0),
    Cell3DPosition(2,-2,0), Cell3DPosition(-2,2,0), Cell3DPosition(2,2,0), };

inline constexpr Cell3DPosition _2ndOrderNeighbors[40] = {
    Cell3DPosition(-2,-2,0), // Bottom-Left corner
    Cell3DPosition(-1,-2,0),
    Cell3DPosition(0,-2,0),
    Cell3DPosition(1,-2,0),

    Cell3DPosition(2,-2,0),  // Bottom-right corner
    Cell3DPosition(2,-1,0),
    Cell3DPosition(2,0,0),
    Cell3DPosition(2,1,0),

    Cell3DPosition(2,2,0), // Top-right corner
    Cell3DPosition(1,2,0),
    Cell3DPosition(0,2,0),
    Cell3DPosition(-1,2,0),

    Cell3DPosition(-2,2,0), // Top-left corner
    Cell3DPosition(-2,1,0),
    Cell3DPosition(-2,0,0),
    Cell3DPosition(-2,-1,0),

    Cell3DPosition(-2,-2,1), // upper ring front-left corner
    Cell3DPosition(-1,-2,1),
    Cell3DPosition(0,-2,1),

    Cell3DPosition(1,-2,1), // upper ring front-right corner
    Cell3DPosition(1,-1,1),
    Cell3DPosition(1,0,1),

    Cell3DPosition(1,1,1), // upper ring rear-right corner
    Cell3DPosition(0,1,1),
    Cell3DPosition(-1,1,1),

    Cell3DPosition(-2,1,1), // upper ring rear-left corner
    Cell3DPosition(-2,0,1),
    Cell3DPosition(-2,-1,1),

    Cell3DPosition(-1,-1,-1), // lower ring front-left corner
    Cell3DPosition(0,-1,-1),
    Cell3DPosition(1,-1,-1),

    Cell3DPosition(2,-1,-1), // lower ring front-right corner
    Cell3DPosition(2,0,-1),
    Cell3DPosition(2,1,-1),

    Cell3DPosition(2,2,-1),  // lower ring rear-right corner
    Cell3DPosition(1,2,-1),
    Cell3DPosition(0,2,-1),

    Cell3DPosition(-1,2,-1), // lower ring rear-left corner
    Cell3DPosition(-1,1,-1),
    Cell3DPosition(-1,0,-1),

    // Cell3DPosition(-1,-1,2), // right above
    // Cell3DPosition(1,1,-2), // right below
};

inline constexpr Cell3DPosition _2ndOrderScaffoldNeighbors[] = {
    Cell3DPosition(-2,-2,0), // Bottom-Left corner
    Cell3DPosition(-1,-2,0),
    Cell3DPosition(0,-2,0),
    Cell3DPosition(1,-2,0),

    Cell3DPosition(2,-2,0),  // Bottom-right corner
    Cell3DPosition(2,-1,0),
    Cell3DPosition(2,0,0),
    Cell3DPosition(2,1,0),

    Cell3DPosition(2,2,0), // Top-right corner
    Cell3DPosition(1,2,0),
    Cell3DPosition(0,2,0),
    Cell3DPosition(-1,2,0),

    Cell3DPosition(-2,2,0), // Top-left corner
    Cell3DPosition(-2,1,0),
    Cell3DPosition(-2,0,0),
    Cell3DPosition(-2,-1,0),

    Cell3DPosition(-2,-2,1), // upper ring front-left corner
    Cell3DPosition(-1,-2,1),
    Cell3DPosition(0,-2,1),

    Cell3DPosition(1,-2,1), // upper ring front-right corner
    Cell3DPosition(1,-1,1),
    Cell3DPosition(1,0,1),

    Cell3DPosition(1,1,1), // upper ring rear-right corner
    Cell3DPosition(0,1,1),
    Cell3DPosition(-1,1,1),

    Cell3DPosition(-2,1,1), // upper ring rear-left corner
    Cell3DPosition(-2,0,1),
    Cell3DPosition(-2,-1,1),

    Cell3DPosition(-1,-1,-1), // lower ring front-left corner
    Cell3DPosition(0,-1,-1),
    Cell3DPosition(1,-1,-1),

    Cell3DPosition(2,-1,-1), // lower ring front-right corner
    Cell3DPosition(2,0,-1),
    Cell3DPosition(2,1,-1),

    Cell3DPosition(2,2,-1),  // lower ring rear-right corner
    Cell3DPosition(1,2,-1),
    Cell3DPosition(0,2,-1),

    Cell3DPosition(-1,2,-1), // lower ring rear-left corner
    Cell3DPosition(-1,1,-1),
    Cell3DPosition(-1,0,-1),

    // Cell3DPosition(-1,-1,2), // right above
    // Cell3DPosition(1,1,-2), // right below
};
#endif // COATING_UTILS__H_
