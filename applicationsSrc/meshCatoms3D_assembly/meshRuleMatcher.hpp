/**
 * @file   meshRuleMatcher.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Fri Jul 13 13:25:47 2018
 *
 * @brief
 *
 *
 */


#ifndef MESH_RULE_MATCHER_HPP_
#define MESH_RULE_MATCHER_HPP_

#include "network.h"
#include "cell3DPosition.h"
#include "catoms3DBlockCode.h"
#include "color.h"

#define MSG_DELAY_MC 5000

static const uint MC_ST_A = 68;
static const uint MC_ST_B = 69;

namespace MeshCoating {

enum BranchIndex { ZBranch, RevZBranch, LZBranch,
                   RZBranch, XBranch, YBranch, N_BRANCHES };
enum AgentRole { FreeAgent, Coordinator, PassiveBeam, ActiveBeamTip, Support};
enum MeshComponent { R, S_Z, S_RevZ, S_LZ, S_RZ,
                     X_1, X_2, X_3, X_4, X_5,
                     Y_1, Y_2, Y_3, Y_4, Y_5,
                     Z_1, Z_2, Z_3, Z_4, Z_5,
                     RevZ_1, RevZ_2, RevZ_3, RevZ_4, RevZ_5,
                     LZ_1, LZ_2, LZ_3, LZ_4, LZ_5,
                     RZ_1, RZ_2, RZ_3, RZ_4, RZ_5,
                     // EPLs == 34 and on
                     RevZ_EPL, // 35
                     RevZ_R_EPL, // 36
                     RZ_L_EPL, // 37
                     RZ_EPL, // 38
                     RZ_R_EPL, // 39
                     Z_R_EPL, // 40
                     Z_EPL, // 41
                     Z_L_EPL, // 42
                     LZ_R_EPL, // 43
                     LZ_EPL, // 44
                     LZ_L_EPL, // 45
                     RevZ_L_EPL }; // 46

class MeshRuleMatcher {
    const int X_MAX, Y_MAX, Z_MAX, B;

public:
    bool isOnXBranch(const Cell3DPosition& pos) const;
    bool isOnXBorder(const Cell3DPosition& pos) const;
    bool isOnXOppBorder(const Cell3DPosition& pos) const;
    bool isOnYBranch(const Cell3DPosition& pos) const;
    bool isOnYBorder(const Cell3DPosition& pos) const;
    bool isOnYOppBorder(const Cell3DPosition& pos) const;
    bool isOnZBranch(const Cell3DPosition& pos) const;
    bool isOnRevZBranch(const Cell3DPosition& pos) const;
    bool isOnRZBranch(const Cell3DPosition& pos) const;
    bool isOnLZBranch(const Cell3DPosition& pos) const;
    bool isTileRoot(const Cell3DPosition& pos) const;
    bool isVerticalBranchTip(const Cell3DPosition& pos) const;
    bool isTileSupport(const Cell3DPosition& pos) const;

    bool shouldGrowBranch(const Cell3DPosition& pos, BranchIndex bi) const;
    bool shouldGrowZBranch(const Cell3DPosition& pos) const;
    bool shouldGrowRevZBranch(const Cell3DPosition& pos) const;
    bool shouldGrowLZBranch(const Cell3DPosition& pos) const;
    bool shouldGrowRZBranch(const Cell3DPosition& pos) const;
    bool shouldGrowXBranch(const Cell3DPosition& pos) const;
    bool shouldGrowYBranch(const Cell3DPosition& pos) const;

    /**
     * Contains the position of each MeshComponent, indexed by their id
     */
    static std::array<Cell3DPosition, 47> componentPosition;

    Cell3DPosition getBranchUnitOffset(int bi) const;
    BranchIndex getBranchIndexForNonRootPosition(const Cell3DPosition& pos);

    MeshRuleMatcher(const uint _X_MAX, const uint _Y_MAX, const uint _Z_MAX,
                                const uint _B) :
        X_MAX(_X_MAX), Y_MAX(_Y_MAX), Z_MAX(_Z_MAX), B(_B) {};
    virtual ~MeshRuleMatcher() {};

    static bool positionIsEPL(const Cell3DPosition& pos);

    bool isInGrid(const Cell3DPosition& pos) const;
    bool isInMesh(const Cell3DPosition& pos) const;
    bool isOnPartialBorderMesh(const Cell3DPosition& pos) const;

        /**
     * For a given position relative to a tile root, returns the corresponding component
     * @param pos relative tile position to evaluate
     * @return the mesh component to which pos corresponds, or -1 if pos not a component
     */
    static int getComponentForPosition(const Cell3DPosition& pos);

    static string roleToString(AgentRole ar);
    static string component_to_string(MeshComponent comp);
    static string branch_to_string(BranchIndex bi);

    bool upwardBranchRulesApply(const Cell3DPosition& own,
                                const Cell3DPosition& other) const;

    bool planarBranchRulesApply(const Cell3DPosition& own,
                                const Cell3DPosition& other) const;

    bool meshRootBranchRulesApply(const Cell3DPosition& own,
                                  const Cell3DPosition& other) const;

    bool partialBorderMeshRulesApply(const Cell3DPosition& own,
                                 const Cell3DPosition& other) const;

    bool shouldSendToNeighbor(const Cell3DPosition& own,
                              const Cell3DPosition& other) const;

    void printDebugInfo(const Cell3DPosition& pos) const;

    /**
     * For a given position, return the branch index of the branch to which it belongs
     * @param pos input position
     * @attention pos should not be tile root position or non-mesh position
     * @return branch index of the branch to which pos belongs, or -1 if invalid
     */
    short determineBranchForPosition(const Cell3DPosition& pos) const;

    /**
     * @param pos position of the module to consider, has to be a valid mesh component
     * @return the position of the tile root to which this module belongs
     * @attention returned position might be out of mesh is pos is an out of mesh component
     */
    const Cell3DPosition getTileRootPositionForMeshPosition(const Cell3DPosition& pos) const;

    /**
     * @param pos position of the module to consider
     * @return the position of the parent module in the spanning tree, or pos if module has no parent
     */
    const Cell3DPosition getTreeParentPosition(const Cell3DPosition& pos) const;

    /**
     * @param pos the module's position
     * @return the number of messages that should be received from the subtree before
     *  propagating a response up the tree
     */
    unsigned int getNumberOfExpectedSubTreeConfirms(const Cell3DPosition& pos) const;

    /**
     * Returns the position of the nearest tile root around pos
     * @param pos
     * @return the position of the nearest tile root
     */
    const Cell3DPosition getNearestTileRootPosition(const Cell3DPosition& pos) const;

    /**
     * Computes the position of the support responsible for module at position pos
     * @param pos position to evaluate. Expects a position that belongs to a branch.
     * @return position of support next to position pos
     */
    const Cell3DPosition getSupportPositionForPosition(const Cell3DPosition& pos) const;

    /**
     * For a catom at position pos, determines which role it should be assigned based on
     *  its position in the mesh structure
     * @param pos position to evaluate
     * @return AgentRole for position pos
     */
    AgentRole getRoleForPosition(const Cell3DPosition& pos) const;

    /**
     * Return the expected position of a catom representing a certain mesh component
     * @param component component to evaluate
     * @return position of module with evaluated role
     */
    const Cell3DPosition getPositionForMeshComponent(MeshComponent component) const;

    /**
     * Return the expected position of a catom representing a certain mesh component,
     *  but for in the relevant child tile of the this tile
     * @param component component to evaluate
     * @return position of module with evaluated component in relevant child tile
     */
    const Cell3DPosition getPositionForChildTileMeshComponent(MeshComponent component) const;

    /**
     * Returns the color assiociated with the AgentRole corresponding with position pos
     * @param pos position to evaluate
     * @return the color assiociated with the AgentRole corresponding with position pos
     */
    const Color& getColorForPosition(const Cell3DPosition& pos) const;

    /**
     * @return a list containing all the location of all tile roots in this mesh,
     *  according to its dimensions
     */
    const vector<Cell3DPosition> getAllGroundTileRootPositionsForMesh() const;

    Cell3DPosition getIndexOfBranchTipUnder(BranchIndex bi) const;

    /**
     * In order to go up one or several tile levels, a catom has to zigzag
     *  onto alternating branch pairs, such as {LZ, RZ, LZ, RZ, ...}, this function
     *  returns the alternate branch for an input branch bi.
     *  (e.g. LZ -> RZ in the previous example)
     * @param bi branch index to evaluate
     * @return the alternate branch index of bi
     */
    BranchIndex getAlternateBranchIndex(BranchIndex bi) const;

    /*********************************************************************/
    /*************************** PYRAMID STUFF ***************************/
    /*********************************************************************/

    bool isOnXPyramidBorder(const Cell3DPosition& pos) const;
    bool isOnXOppPyramidBorder(const Cell3DPosition& pos) const;
    bool isOnYPyramidBorder(const Cell3DPosition& pos) const;
    bool isOnYOppPyramidBorder(const Cell3DPosition& pos) const;

    /**
     * @param pos position to evaluate
     * @return true if pos is part of the mesh pyramid
     */
    bool isInPyramid(const Cell3DPosition& pos) const;

    /**
     * Returns the position of the tile root at the end of branch bi for tile root
     *  at posiion trRef
     * @param trRef position of the tile root relative to which the answer is asked
     * @param bi branch to evaluate
     * @return the position of the tile root at the end of branch bi for tile root
     *  at posiion trRef
     */
    const Cell3DPosition getTileRootAtEndOfBranch(const Cell3DPosition& trRef,
                                                  BranchIndex bi) const;

    /**
     * Like shouldGrowBranch, but also takes into account whether branch would be outside
     *  of the mesh pyramid
     * @param pos
     * @param bi
     * @return
     */
    bool shouldGrowPyramidBranch(const Cell3DPosition& pos, BranchIndex bi) const;

    /**
     * Checks whether module at the tip of branch tipB relative to tile root at position pos
     *  should grow branch growthB according to pyramid and mesh rules.
     * @param pos position of the source tile root
     * @attention pos must be a tile root
     * @param tipB branch whose tip to consider
     * @param growthB branch that tip TR should consider growing
     * @return true if TR at tip of branch tipB should grow branch growthB, false otherwise.
     */
    bool pyramidTRAtBranchTipShouldGrowBranch(const Cell3DPosition& pos,
                                              BranchIndex tipB, BranchIndex growthB) const;
};

}
#endif /* MESH_RULE_MATCHER_HPP_ */
