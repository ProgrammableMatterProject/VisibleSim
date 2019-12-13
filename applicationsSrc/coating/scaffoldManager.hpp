#ifndef ScaffoldManager_H_
#define ScaffoldManager_H_

#include "lattice.h"
#include "catoms3DBlock.h"

#include <list>

using namespace Catoms3D;

enum BranchIndex {
    ZBranch, RevZBranch, LZBranch,
    RZBranch, XBranch, YBranch,
    OppXBranch, OppYBranch,
    N_BRANCHES
};

class ScaffoldManagerException : public VisibleSimException {
public:
    ScaffoldManagerException()
        : VisibleSimException(std::string("Unknown scaffold manager exception")) {}
    ScaffoldManagerException(const std::string &reason)
        : VisibleSimException(TermColor::BRed + std::string("error (ScaffoldManager): ")
                              + TermColor::Reset + reason) {}

};

class ScaffoldManager {
private:
    const Cell3DPosition seed; // Position of the CSG seed tile coordinator
    inline static const int B = 6;
    Lattice *lattice;
    const std::function<bool(const Cell3DPosition&)> isInCSG;
public :
    ScaffoldManager(const Cell3DPosition& _seed,
                    std::function<bool(const Cell3DPosition&)> _isInCSG);
    ~ScaffoldManager() {};

    /**
     * Position of all the tips of the incoming branches to a tile relative to its tile root
     */
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

    inline const Cell3DPosition& getIncidentTipRelativePos(BranchIndex bi) const {
        return incidentTipRelativePos[bi];
    }

    /**
     * The scaffold manager uses a coodinate system whereby the origin is at the position of
     *  the scaffold seed. This function normalizes a real position into a scaffold one.
     * @param pos
     * @return the transposed position of pos into the scaffold coordinate system.
     */
    Cell3DPosition normalize(const Cell3DPosition& pos) const;

    /**
     * Inverse operation of ScaffoldManager::normalize.
     * @param pos
     * @return the transposed position of pos into the real coordinate system.
     */
    Cell3DPosition denormalize(const Cell3DPosition& pos) const;

    /**
     * @param pos
     * @return true if position pos is at a tile root position
     */
    bool isTileRoot(const Cell3DPosition& pos) const;

    /**
     * @param pos
     * @return true if position pos is in the scaffold
     */
    bool isInScaffold(const Cell3DPosition& pos) const;

    /**
     * @param pos
     * @return true if position pos is a helper position (previously named support module)
     */
    bool isHelperModule(const Cell3DPosition& pos) const;

    /**
     * @param bi
     * @param pos
     * @return true if position pos is on branch bi
     */
    bool isOnBranch(BranchIndex bi, const Cell3DPosition& pos) const;

    /**
     * @param bi
     * @return the unit vector expressing the direction of branch bi
     */
    Cell3DPosition getBranchUnitOffset(int bi) const;

    /**
     * @param trRef tile root position as reference
     * @param bi branch to consider
     * @param outgoing whether bi is an ingoing or outgoing branch
     * @return the tile root that is at the end (ingoing or outgoing) of root at trRef
     */
    const Cell3DPosition getTileRootAtEndOfBranch(const Cell3DPosition& trRef,
                                                  BranchIndex bi, bool outgoing = true) const;

    /**
     * @param pos tile root position of tile to consider
     * @param bi branch to evaluate
     * @return true is tile at pos has an incident branch bi
     */
    bool hasIngoingBranch(const Cell3DPosition& pos, BranchIndex bi) const;

    /**
     * @param bi
     * @return the alternate branch for branch bi, that is to say the index of the branch
     *  that is directly overhead---e.g., RevZ for Z, LZ for RZ, etc...
     */
    BranchIndex getAlternateBranchIndex(BranchIndex bi) const;

    /**
     * @param pos tile root position of tile to consider
     * @param bi
     * @return true if tile at location pos should grow at least one module from branch bi
     */
    bool shouldGrowBranch(const Cell3DPosition& pos, BranchIndex bi) const;

    /**
     * @param pos tile root position of the tile to consider
     * @param bi
     * @return the number of modules that tile pos has to grow for branch bi
     */
    short resourcesForBranch(const Cell3DPosition& pos, BranchIndex bi) const;

    /**
     * Lists all structural support positions for the plane z of the scaffold.
     *  Structural supports are modules that are added at the tip of border horizontal branches
     *  to solidarize the coating to the scaffold.
     * @param z the eleveation of the plane to consider in the real coordinate system.
     * @return an empty list if plane Z is not a tile root plane, or the list of structural
     *  positions for that plane otherwise.
     */
    list<Cell3DPosition> getAllSupportPositionsForPlane(int z) const;

    static string branch_to_string(BranchIndex bi);
};

#endif /* ScaffoldManager_H_ */
