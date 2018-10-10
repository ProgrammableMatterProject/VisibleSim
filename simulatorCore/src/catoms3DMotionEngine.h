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

#include "catoms3DMotionRules.h"
#include "catoms3DWorld.h"

namespace Catoms3D {

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
    static const Catoms3DMotionRulesLink* findConnectorLink(Catoms3DBlock *module,
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
    static const Catoms3DMotionRulesLink* findPivotConnectorLink(Catoms3DBlock *pivot,
                                                                 short conFrom, short conTo,
                                                                 RotationLinkType ft);

    /** 
     * Computes and return the mirror connector of _mirroringCon_ of m1, on the surface of m2 with m1 and m2 connected through the connector of id _dockingConM1_  and _dockingConM2_, of m1 and m2, respectively.
     * @note If m1 was to rotate from its _mirroringCon_ connector to its _dockingConM1_ connector using m2 as pivot, the mirror connector of _mirroringCon_ corresponds to the connector of m2 on which m1 is now attached.
     * @param m1 reference module. Module that wants to move.
     * @param m2 pivot module
     * @param dockingConM1 connector through which m1 is attached to m2 (belongs to m1).
     * @param dockingConM2 connector through which m2 is attached to m1 (belongs to m2).
     * @param mirroringCon connector to be mirrored on m2 (belongs to m1).
     * @return mirror connector of dockingCon on m2 (belongs to m2), or -1 if the two connectors are not neighbors (not connected through a face).
     */
    static short getMirrorConnectorOnModule(Catoms3DBlock *m1, Catoms3DBlock *m2,
                                            short dockingConM1, short dockingConM2,
                                            short mirroringCon);
};

};

#endif // __CATOMS3D_MOTION_ENGINE_H__
