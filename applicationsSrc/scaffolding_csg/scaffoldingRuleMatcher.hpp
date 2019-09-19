/**
 * @file   scaffoldingRuleMatcher.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jun 11 15:32:41 2019
 *
 * @brief
 *
 *
 */


#ifndef SCAFFOLD_RULE_MATCHER_HPP_
#define SCAFFOLD_RULE_MATCHER_HPP_

#include <array>
#include <functional>

#include "network.h"
#include "cell3DPosition.h"
#include "catoms3DBlockCode.h"
#include "color.h"

#define MSG_DELAY_MC 5000

static const uint MC_ST_A = 68;
static const uint MC_ST_B = 69;

namespace MeshCoating {

enum BranchIndex { ZBranch, RevZBranch, LZBranch,
    RZBranch, XBranch, YBranch,
    OppXBranch, OppYBranch,
    N_BRANCHES };
enum AgentRole { FreeAgent, Coordinator, PassiveBeam, ActiveBeamTip, Support};
enum ScafComponent { R, S_Z, S_RevZ, S_LZ, S_RZ,

                     X_1, X_2, X_3, X_4, X_5,
                     Y_1, Y_2, Y_3, Y_4, Y_5,

                     Z_1, Z_2, Z_3, Z_4, Z_5,
                     RevZ_1, RevZ_2, RevZ_3, RevZ_4, RevZ_5,
                     LZ_1, LZ_2, LZ_3, LZ_4, LZ_5,
                     RZ_1, RZ_2, RZ_3, RZ_4, RZ_5,

                     OPP_X1, OPP_X2, OPP_X3, OPP_X4, OPP_X5,
                     OPP_Y1, OPP_Y2, OPP_Y3, OPP_Y4, OPP_Y5,

                     OPP_Z1, OPP_Z2, OPP_Z3, OPP_Z4, OPP_Z5,
                     OPP_RevZ1, OPP_RevZ2, OPP_RevZ3, OPP_RevZ4, OPP_RevZ5,
                     OPP_LZ1, OPP_LZ2, OPP_LZ3, OPP_LZ4, OPP_LZ5,
                     OPP_RZ1, OPP_RZ2, OPP_RZ3, OPP_RZ4, OPP_RZ5,

                     // EPLs == 64 and on
                     RevZ_EPL, // 65
                     RevZ_R_EPL, // 66
                     RZ_L_EPL, // 67
                     RZ_EPL, // 68
                     RZ_R_EPL, // 69
                     Z_R_EPL, // 40
                     Z_EPL, // 41
                     Z_L_EPL, // 42
                     LZ_R_EPL, // 43
                     LZ_EPL, // 44
                     LZ_L_EPL, // 45
                     RevZ_L_EPL, // 46

                     OPP_X_R_EPL,
                     OPP_Y_L_EPL,
                     OPP_Y_R_EPL,
                     X_L_EPL,
                     X_R_EPL,
                     Y_L_EPL,
                     Y_R_EPL,
                     OPP_X_L_EPL,

                     N_COMPONENTS
};

class ScaffoldingRuleMatcher {
    const int X_MAX, Y_MAX, Z_MAX;
    const int X_MIN, Y_MIN, Z_MIN;
    const int B;
    const Cell3DPosition seed; // Position of the CSG seed tile coordinator
    const std::function<bool(const Cell3DPosition)> isInsideFn;

    /**
     * Contains the position of each ScafComponent, indexed by their id
     */
    static std::array<Cell3DPosition, N_COMPONENTS> componentPosition;

    inline static constexpr std::array<Cell3DPosition, N_BRANCHES> incidentTipRelativePos =
    {
        Cell3DPosition(0,0,-1), // ZBranch
        Cell3DPosition(1,1,-1), // RevZBranch
        Cell3DPosition(1,0,-1), // LZBranch
        Cell3DPosition(0,1,-1), // RZBranch
        Cell3DPosition(-1,0,0), // XBranch
        Cell3DPosition(0,-1,0), // YBranch
        Cell3DPosition(1,0,0), // XOppBranch
        Cell3DPosition(0,1,0) // YOppBranch
    };

    inline static constexpr std::array<Cell3DPosition, 20> entryPointRelativePos =
    {
        Cell3DPosition(-1,-1,-1), // RevZ_EPL
        Cell3DPosition(0,-1,-1), // RevZ_R_EPL
        Cell3DPosition(1,-1,-1), // RZ_L_EPL
        Cell3DPosition(2,-1,-1), // RZ_EPL

        Cell3DPosition(2,0,-1), // RZ_R_EPL
        Cell3DPosition(2,1,-1), // Z_R_EPL
        Cell3DPosition(2,2,-1), // Z_EPL

        Cell3DPosition(1,2,-1), // Z_L_EPL
        Cell3DPosition(0,2,-1), // LZ_R_EPL
        Cell3DPosition(-1,2,-1), // LZ_EPL

        Cell3DPosition(-1,1,-1), // LZ_L_EPL
        Cell3DPosition(-1,0,-1), // RevZ_L_EPL

        Cell3DPosition(-2, -1, 0), // OPP_X_R_EPL
        Cell3DPosition(-1, -2, 0), // OPP_Y_L_EPL
        Cell3DPosition(1, -2, 0), // OPP_Y_R_EPL
        Cell3DPosition(2, -1, 0), // X_L_EPL
        Cell3DPosition(2, 1, 0), // X_R_EPL
        Cell3DPosition(1, 2, 0), // Y_L_EPL
        Cell3DPosition(-1, 2, 0), // Y_R_EPL
        Cell3DPosition(-2, 1, 0), // OPP_X_L_EPL
    };
public:

    ScaffoldingRuleMatcher(const uint _X_MAX, const uint _Y_MAX, const uint _Z_MAX,
                           const uint _X_MIN, const uint _Y_MIN, const uint _Z_MIN,
                           const uint _B,
                           const std::function<bool(const Cell3DPosition&)>_fn):
        X_MAX(_X_MAX), Y_MAX(_Y_MAX), Z_MAX(_Z_MAX),
        X_MIN(_X_MIN), Y_MIN(_Y_MIN), Z_MIN(_Z_MIN),
        B(_B), isInsideFn(_fn) {};
    virtual ~ScaffoldingRuleMatcher() {};

    bool isOnBranch(BranchIndex bi, const Cell3DPosition& pos) const;

    bool isOnXBranch(const Cell3DPosition& pos) const;
    bool isOnYBranch(const Cell3DPosition& pos) const;
    bool isOnZBranch(const Cell3DPosition& pos) const;
    bool isOnRevZBranch(const Cell3DPosition& pos) const;
    bool isOnRZBranch(const Cell3DPosition& pos) const;
    bool isOnLZBranch(const Cell3DPosition& pos) const;

    bool isOnOppYBranch(const Cell3DPosition& pos) const;
    bool isOnOppXBranch(const Cell3DPosition& pos) const;
    bool isOnOppZBranch(const Cell3DPosition& pos) const;
    bool isOnOppRevZBranch(const Cell3DPosition& pos) const;
    bool isOnOppRZBranch(const Cell3DPosition& pos) const;
    bool isOnOppLZBranch(const Cell3DPosition& pos) const;

    bool isTileRoot(const Cell3DPosition& pos) const;
    bool isVerticalBranchTip(const Cell3DPosition& pos) const;
    bool isNFromVerticalBranchTip(const Cell3DPosition& pos, int N) const;
    bool isSupportModule(const Cell3DPosition& pos) const;
    bool isBranchModule(const Cell3DPosition& pos) const;
    bool isZBranchModule(const Cell3DPosition& pos) const;
    bool isEPLPivotModule(const Cell3DPosition& pos) const;

    bool isOnHorizontalEPL(const Cell3DPosition& pos) const;

    /**
     * Indicates how many modules are in branch bi for tile at pos
     * @param pos tile root position of reference
     * @param bi target branch
     * @return 0 if branch should not be grown, or n \in [1,B-1] otherwise
     */
    short resourcesForBranch(const Cell3DPosition& pos,
                             BranchIndex bi) const;

    bool shouldGrowBranch(const Cell3DPosition& pos, BranchIndex bi) const;

    Cell3DPosition getBranchUnitOffset(int bi) const;
    Cell3DPosition getBranchUnitOffset(const Cell3DPosition& pos) const;
    BranchIndex getBranchIndexForNonRootPosition(const Cell3DPosition& pos) const;

    static string roleToString(AgentRole ar);
    static string component_to_string(ScafComponent comp);
    static string branch_to_string(BranchIndex bi);

    bool isInGrid(const Cell3DPosition& pos) const;
    bool isInMesh(const Cell3DPosition& pos) const;
    bool isInCSGMeshOrSandbox(const Cell3DPosition& pos) const;
    bool isInSandbox(const Cell3DPosition& pos) const;

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

    /*
     * @param root position of the tile root of the tile to consider
     * @param pos position of the module to consider, has to be a valid mesh component
     * @return true if pos belongs to the tile whose root is in argument
     */
    bool isInTileWithRootAt(const Cell3DPosition& root, const Cell3DPosition& pos) const;

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
    static const Cell3DPosition getPositionForScafComponent(ScafComponent component);

    /**
     * Return the expected position of a catom representing a certain mesh component,
     *  but for in the relevant child tile of the this tile
     * @param component component to evaluate
     * @return position of module with evaluated component in relevant child tile
     */
    const Cell3DPosition getPositionForChildTileScafComponent(ScafComponent component) const;

    /**
     * Returns the color assiociated with the AgentRole corresponding with position pos
     * @param pos position to evaluate
     * @return the color assiociated with the AgentRole corresponding with position pos
     */
    const Color& getColorForPosition(const Cell3DPosition& pos) const;

    static Cell3DPosition getPositionOfBranchTipUnder(BranchIndex bi);

    /**
     * In order to go up one or several tile levels, a catom has to zigzag
     *  onto alternating branch pairs, such as {LZ, RZ, LZ, RZ, ...}, this function
     *  returns the alternate branch for an input branch bi.
     *  (e.g. LZ -> RZ in the previous example)
     * @param bi branch index to evaluate
     * @return the alternate branch index of bi
     */
    BranchIndex getAlternateBranchIndex(BranchIndex bi) const;

    /**
     * For a given mesh component, return its position relative to its tile root
     * @param comp component to evaluate
     * @return the position of component comp in the scaffold
     */
    static Cell3DPosition getPositionForComponent(ScafComponent comp);

    /**
     * For a given position relative to a tile root, returns the corresponding component
     * @param pos relative tile position to evaluate
     * @return the mesh component to which pos corresponds, or -1 if pos not a component
     */
    static int getComponentForPosition(const Cell3DPosition& pos);

    /**
     * Returns the ScafComponent corresponding to the central or default EPL
     *  component for the input branch
     * @param bi input branch id to evaluate, must be a valid BranchIndex value
     * @return default EPL component for branch bi
     */
    static ScafComponent getDefaultEPLComponentForBranch(BranchIndex bi);

    /**
     * For a given input branch, return the branch EPL that modules should normally
     *  be forwarded to (Alternate branch EPL of branch of above bi)
     * @param bi input branch id to evaluate, must be a valid BranchIndex value
     * @return target EPL component for a wandering module on branch bi
     */
    static ScafComponent getTargetEPLComponentForBranch(BranchIndex bi);

    /**
     * For a given EPL component, return the branch index on which this EPL is located
     * @param epl any EPL component, must be a valide ScafComponent EPL value
     * @return the branch index on which epl is located
     */
    static BranchIndex getBranchForEPL(ScafComponent epl);

    /**
     * For a given branch, returns the position of the default EPL that wandering modules
     *  normally move to from that branch
     * @see ScaffoldingRuleMatcher::getDefaultEPLComponentForBranch
     * @param bi input branch id to evaluate, must be a valid BranchIndex value
     * @return position of the default EPL target for branch bi
     */
    static const Cell3DPosition getTargetEPLPositionForBranch(BranchIndex bi);

    /**
     * Returns the branch index of a non EPL mesh component
     * @param mc input non EPL mesh component
     * @return branch index of component mc, or -1 if undefined
     * @attention for EPL components, use @see ScaffoldingRuleMatcher::getBranchForEPL
     */
    static int getBranchIndexForScafComponent(ScafComponent mc);


    /**
     * Returns the position of the tile root at the end of branch bi for tile root
     *  at posiion trRef
     * @param trRef position of the tile root relative to which the answer is asked
     * @param bi branch to evaluate
     * @param upward whether to go up or down from tile root, true = up
     * @return the position of the tile root at the end of branch bi for tile root
     *  at posiion trRef
     */
    const Cell3DPosition getTileRootAtEndOfBranch(const Cell3DPosition& trRef,
                                                  BranchIndex bi,
                                                  bool upward = true) const;

    /**
     * Indicates whether a tile has an incident branch on index bi
     * @param pos position of a tile root in the scaffold
     * @param bi branch to evaluate
     * @return true if pos is a TR position and TR at lower end of branch bi exists
     */
    bool hasIncidentBranch(const Cell3DPosition& pos, BranchIndex bi) const;

    inline const Cell3DPosition& getEntryPointRelativePos(short i) const {
        return entryPointRelativePos[i];
    }

    inline const Cell3DPosition& getEntryPointRelativePos(ScafComponent mc) const {
        VS_ASSERT_MSG(mc >= RevZ_EPL, "Attempting to getEntryPointRelativePos for non EPL component");
        return entryPointRelativePos[mc - RevZ_EPL];
    }

    inline const Cell3DPosition& getIncidentTipRelativePos(BranchIndex bi) const {
        return incidentTipRelativePos[bi];
    }

    /*
     * @param pos1 first position
     * @param pos2 second position to compare
     * @return true pos1 and pos2 are on the same branch of the same tile
     */
    bool areOnTheSameBranch(const Cell3DPosition& pos1, const Cell3DPosition& pos2) const;


    /**
     * Computes the entry point direction identifier corresponding to neighbor cell pos
     * @param pos a cell that is adjacent to the current module
     * @attention pos is not a relative position but an absolute one!
     * @return entry point direction id corresponding to pos or -1 if pos not adjacent to module or if cell is not on a module's lower connectors
     */
    short getEntryPointLocationForCell(const Cell3DPosition& pos);

    /**
     * @param epl Entry point location name of the desired entry point
     * @param cPos position of the coordinator caller
     * @return absolute position of the requested entry point location
     */
    const Cell3DPosition getEntryPointPosition(const Cell3DPosition& cPos,
                                               ScafComponent epl) const;

    /**
     * Indicates whether position pos is an entry point location
     * @param pos position to evaluate
     * @return true if pos is an EPL, false otherwise
     */
    bool isOnEntryPoint(const Cell3DPosition& pos) const;


    /**
     * Finds an entry point index for a catom required to fill component mc
     * @param cPos position of the coordinator caller
     * @param mc mesh component type of the catom
     * @return entry point position / spawn location for component mc
     */
    const Cell3DPosition getEntryPointForScafComponent(const Cell3DPosition& cPos,
                                                       ScafComponent mc);

    const Cell3DPosition getEntryPointForModuleOnIncidentBranch(const Cell3DPosition& cPos,
                                                                BranchIndex bid);


    /*********************************************************************/
    /*************************** CSG STUFF ***************************/
    /*********************************************************************/

    bool isOnXCSGBorder(const Cell3DPosition& pos) const;
    bool isOnXOppCSGBorder(const Cell3DPosition& pos) const;
    bool isOnYCSGBorder(const Cell3DPosition& pos) const;
    bool isOnYOppCSGBorder(const Cell3DPosition& pos) const;

    /**
     * @param pos position to evaluate
     * @return true if pos is part of the CSG target object
     */
    bool isInCSG(const Cell3DPosition& pos) const;

    /**
     * Checks whether module at the tip of branch tipB relative to tile root at position pos
     *  should grow branch growthB according to csg and mesh rules.
     * @param pos position of the source tile root
     * @attention pos must be a tile root
     * @param tipB branch whose tip to consider
     * @param growthB branch that tip TR should consider growing
     * @return true if TR at tip of branch tipB should grow branch growthB, false otherwise.
     */
    bool CSGTRAtBranchTipShouldGrowBranch(const Cell3DPosition& pos,
                                          BranchIndex tipB, BranchIndex growthB) const;

    /**
     * Indicates the number of incident branches for tile whose TR is at pos
     * @param pos position of concerned TR
     * @param lambda a filtering function that returns whether a position is within the
     *  scaffold for a particular type of object
     * @return the number of vertical branches incident to the specified tile,
     *  or -1 if the input is invalid
     */
    short getNbIncidentVerticalBranches(const Cell3DPosition& pos) const;

    /**
     * Returns the coordinates of the first TR along the xy diagonal of the CSG object
     *  at tile layer z
     * @param z
     * @remark z is the tile level, which means that z = z_real / B
     * @return position of the seed for plane z
     */
    Cell3DPosition getSeedForCSGLayer(int z) const;

    /**
     * Returns, for a given tile position, which branch index should bring the tile root
     * @param pos position of the tile
     * @return branch index of the branch that should bring the tile root
     */
    BranchIndex getTileRootInsertionBranch(const Cell3DPosition& pos) const;
};

}
#endif /* MESH_RULE_MATCHER_HPP_ */
