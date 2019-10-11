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
enum CCWDir {FrontLeft, Front, FrontRight, Right, RearRight, Rear, RearLeft, Left };

inline const int NumCCWDirs = 8;
inline constexpr Cell3DPosition CCWDPos[NumCCWDirs] = {
    Cell3DPosition(-1, -1, 0), // FrontLeft
    Cell3DPosition(0, -1, 0), // Front
    Cell3DPosition(1, -1, 0), // FrontRight
    Cell3DPosition(1, 0, 0), // Right
    Cell3DPosition(1, 1, 0), // RearRight
    Cell3DPosition(0, 1, 0), // Rear
    Cell3DPosition(-1, 1, 0), // RearLeft
    Cell3DPosition(-1, 0, 0), // Left
};

inline constexpr Cell3DPosition diagNeighbors[4] = { Cell3DPosition(-1,-1,0),
    Cell3DPosition(1,-1,0), Cell3DPosition(-1,1,0), Cell3DPosition(1,1,0), };

inline constexpr Cell3DPosition _2ndOrderNeighbors[16] = {
    Cell3DPosition(-2,-2,0), // Bottom-Left corner
    Cell3DPosition(-1,-2,0),
    Cell3DPosition(-0,-2,0),
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
};

#endif // COATING_UTILS__H_
