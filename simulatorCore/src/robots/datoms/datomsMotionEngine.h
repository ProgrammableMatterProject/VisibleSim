/**
 * @file   datomsMotionEngine.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Oct 10 12:57:01 2018
 *
 * @brief  Helper functions for planning Datoms deformation
 *
 *
 */

#ifndef __DATOMS_MOTION_ENGINE_H__
#define __DATOMS_MOTION_ENGINE_H__

#include <utility>

#include "datomsMotionRules.h"
#include "datomsWorld.h"

namespace Datoms {

class DatomsMotionEngine {
    // FIXME: World is a poor container for this
    static inline DatomsMotionRules* getMotionRules() {
        return DatomsWorld::getWorld()->getMotionRules();
    }
public:
    // /**
    //    @brief Given a set of motion rules link passed as argument, searches a path (sequence of individual rotations) that leads from connector conFrom to connector conTo
    //    @param motionRulesLinks a set of surface links between connectors of a pivot module that another module can follow to rotate
    //    @param conFrom the source connector of the desired connector path
    //    @param conTo the destination connector of the desired connector path
    //    @return an ordered list of individual links that can be followed by a module to move from conFrom to conTo, or list.end() if no path has been found
    //    @remarks Fastest path is found through BFS traversal of the connector graph */
    // static DatomsMotionRulesLink* findSurfaceConnectorPath(const vector<DatomsMotionRulesLink*>& motionRulesLinks,
    //                                                          short conFrom,
    //                                                          short conTo,
    //                                                          DatomsBlock *catom);


    /**
     * Searches for a connector path that can be followed by a mobile module.
     * @param module mobile module
     * @param conFrom connectors on which the module is currently attached to the pivot
     * @param conTo connector on which the module seeks to attach to the pivot after rotating
     * @param ft can be used to specify which face to prefer. The policy is that no link is returned if the motion is not possible using the link type specified by ft
     * @attention @todo This function does not currently check for further blocking modules
     * @return a connector link that can be used for the desired motion if it exists, NULL otherwise
     */
    static const DatomsMotionRulesLink* findConnectorLink(const DatomsBlock *module,
                                                            short conFrom, short conTo);

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
    static const DatomsMotionRulesLink* findPivotConnectorLink(const DatomsBlock *pivot,
                                                                 short conFrom, short conTo);

    /**
     * Attempts to find all pairs of pivot and connector link on that pivot that would allow
     *  module m to rotate to position tPos under face requirement faceReq
     * @param m module attempting the motion
     * @param tPos target location of the motion
     * @param faceReq if specified, until searches for rotations using one
     *  type of face of the module
     * @return a vector of {pivot, link} pair representing the possible motions
     */
    /*static std::vector<std::pair<DatomsBlock*, const DatomsMotionRulesLink*>>
    findPivotLinkPairsForTargetCell(const DatomsBlock* m, const Cell3DPosition& tPos);*/

    /**
        \brief Tries to find a neighbor module of m that can be used as a pivot to move m to tPos
        \param m mobile that should move
        \param tPos target position of m
        \param faceReq if specified, only pivots that can perform a motion using the requested face type will be evaluated
        \return a pointer to a potential pivot, or NULL if none exist
        \todo Implement function
    **/
    /*static DatomsBlock*
    findMotionPivot(const DatomsBlock* m, const Cell3DPosition& tPos);*/

    /**
        \brief Computes a list of all possible rotations for module m
        \param m module to evaluate
        \return a vector containing all possible deformations for datom m
    **/
    static const vector<std::pair<const DatomsMotionRulesLink*, Deformation>>
    getAllDeformationsForModule(const DatomsBlock* m);
};

}

#endif // __DATOMS_MOTION_ENGINE_H__
