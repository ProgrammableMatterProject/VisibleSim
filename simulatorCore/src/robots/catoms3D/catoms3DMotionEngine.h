/**
 * @file   catoms3DMotionEngine.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Oct 10 12:57:01 2018
 *
 * @brief  Helper functions for planning Catoms3D rotations
 *
 *
 */

#ifndef __CATOMS3D_MOTION_ENGINE_H__
#define __CATOMS3D_MOTION_ENGINE_H__

#include <utility>

#include "catoms3DMotionRules.h"
#include "catoms3DWorld.h"

namespace Catoms3D {

inline constexpr Cell3DPosition relReachablePosition[18] = {
    // -2
    Cell3DPosition(1,1,-2),

    // -1
    Cell3DPosition(0,0,-1),
    Cell3DPosition(0,1,-1),
    Cell3DPosition(1,0,-1),
    Cell3DPosition(1,1,-1),

    // 0
    Cell3DPosition(-1, -1, 0), // FrontLeft
    Cell3DPosition(0, -1, 0), // Front
    Cell3DPosition(1, -1, 0), // FrontRight
    Cell3DPosition(1, 0, 0), // Right
    Cell3DPosition(1, 1, 0), // RearRight
    Cell3DPosition(0, 1, 0), // Rear
    Cell3DPosition(-1, 1, 0), // RearLeft
    Cell3DPosition(-1, 0, 0), // Left

    // 1
    Cell3DPosition(0,0,1),
    Cell3DPosition(0,-1,1),
    Cell3DPosition(-1,0,1),
    Cell3DPosition(-1,-1,1),

    // 2
    Cell3DPosition(-1, -1, 2),
};


class Catoms3DMotionEngine {
    // FIXME: World is a poor container for this
    static inline Catoms3DMotionRules* getMotionRules() {
        return Catoms3DWorld::getWorld()->getMotionRules();
    }
public:
    // /**
    //    @brief Given a set of motion rules link passed as argument, searches a path (sequence of individual rotations) that leads from connector conFrom to connector conTo
    //    @param motionRulesLinks a set of surface links between connectors of a pivot module that another module can follow to rotate
    //    @param conFrom the source connector of the desired connector path
    //    @param conTo the destination connector of the desired connector path
    //    @return an ordered list of individual links that can be followed by a module to move from conFrom to conTo, or list.end() if no path has been found
    //    @remarks Fastest path is found through BFS traversal of the connector graph */
    // static Catoms3DMotionRulesLink* findSurfaceConnectorPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
    //                                                          short conFrom,
    //                                                          short conTo,
    //                                                          Catoms3DBlock *catom);


    /**
     * Searches for a connector path that can be followed by a mobile module.
     * @param module mobile module
     * @param conFrom connectors on which the module is currently attached to the pivot
     * @param conTo connector on which the module seeks to attach to the pivot after rotating
     * @param ft can be used to specify which face to prefer. The policy is that no link is returned if the motion is not possible using the link type specified by ft
     * @attention @todo This function does not currently check for further blocking modules
     * @return a connector link that can be used for the desired motion if it exists, NULL otherwise
     */
    static const Catoms3DMotionRulesLink* findConnectorLink(const Catoms3DBlock *module,
                                                            short conFrom, short conTo,
                                                            RotationLinkType ft);

    /**
     * Same as findConnectorLink, but with planning directly using the connectors of the pivot
     *
     * @param pivot
     * @param conFrom
     * @param conTo
     * @param ft
     * @attention DO NOT USE FOR NOW
     * @deprecated
     * @return
     */
    static const Catoms3DMotionRulesLink* findPivotConnectorLink(const Catoms3DBlock *pivot,
                                                                 short conFrom, short conTo,
                                                                 RotationLinkType ft);

    /**
     * Computes and return the mirror connector of mirroringCon of m1, on the surface of m2 with m1 and m2 connected through the connector of id dockingConM1  and dockingConM2, of m1 and m2, respectively.
     * @note If m1 was to rotate from its mirroringCon connector to its dockingConM1 connector using m2 as pivot, the mirror connector of mirroringCon corresponds to the connector of m2 on which m1 is now attached.
     * @param m1 reference module. Module that wants to move.
     * @param m2 pivot module
     * @param dockingConM1 connector through which m1 is attached to m2 (belongs to m1).
     * @param dockingConM2 connector through which m2 is attached to m1 (belongs to m2).
     * @param mirroringCon connector to be mirrored on m2 (belongs to m1).
     * @return mirror connector of dockingCon on m2 (belongs to m2), or -1 if the two connectors are not neighbors (not connected through a face).
     */
    static short getMirrorConnectorOnModule(const Catoms3DBlock *m1, const Catoms3DBlock *m2,
                                            short dockingConM1, short dockingConM2,
                                            short mirroringCon);


    /**
     * Attempts to find all pairs of pivot and connector link on that pivot that would allow
     *  module m to rotate to position tPos under face requirement faceReq
     * @param m module attempting the motion
     * @param tPos target location of the motion
     * @param faceReq if specified, until searches for rotations using one
     *  type of face of the module
     * @return a vector of {pivot, link} pair representing the possible motions
     */
    static std::vector<std::pair<Catoms3DBlock*, const Catoms3DMotionRulesLink*>>
    findPivotLinkPairsForTargetCell(const Catoms3DBlock* m, const Cell3DPosition& tPos,
                                    RotationLinkType faceReq = RotationLinkType::Any);

    /**
     * Tries to find a neighbor module of m that can be used as a pivot to move m to tPos
     * @param m mobile that should move
     * @param tPos target position of m
     * @param faceReq if specified, only pivots that can perform a motion using the
     *  requested face type will be evaluated
     * @return a pointer to a potential pivot, or NULL if none exist
     * @todo Implement function
     */
    static Catoms3DBlock*
    findMotionPivot(const Catoms3DBlock* m, const Cell3DPosition& tPos,
                    RotationLinkType faceReq = RotationLinkType::Any);

    /**
     * Computes a list of all possible rotations for module m
     * @param m module to evaluate
     * @return a vector containing all possible rotations for catom
     */
    static const vector<std::pair<const Catoms3DMotionRulesLink*, Catoms3DRotation>>
    getAllRotationsForModule(const Catoms3DBlock* m);

    /**
     * Returns true if module m can move to position tpos using face type faceReq
     * @param m
     * @param tPos
     * @param faceReq
     * @remark this is equivalent to checking if there is a pivot that can bring m to tpos
     * @return true if module m can move to position tpos using face type faceReq, false otherwise
     */
    static bool canMoveTo(const Catoms3DBlock* m, const Cell3DPosition& tPos,
                          RotationLinkType faceReq = RotationLinkType::Any);

    /**
     * Returns a list of all positions that a given module can reach in a single rotation
     * @param m module to evaluate
     * @param faceReq
     * @return a list of all positions that a given module can reach in a single rotation
     */
    static const vector<Cell3DPosition>
    getAllReachablePositions(const Catoms3DBlock* m,
                             RotationLinkType faceReq = RotationLinkType::Any);

    /**
     * Check if a 3D Catoms can reach the final position from the origin one
     * @param origin initial position of the module
     * @param final final position after one motion
     */
    static bool isNotLockedForMotion(const Cell3DPosition &origin,const Cell3DPosition &final, bool isHexaFace);
};

}

#endif // __CATOMS3D_MOTION_ENGINE_H__
