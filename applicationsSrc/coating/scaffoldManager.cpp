#include "scaffoldManager.hpp"

#include "world.h"

#include <string>

ScaffoldManager::ScaffoldManager(const Cell3DPosition& _seed,
                                 std::function<bool(const Cell3DPosition&)> _isInCSG)
    : seed(_seed), isInCSG(_isInCSG) {

    lattice = BaseSimulator::getWorld()->lattice;
    isInsideFn = std::bind(&ScaffoldManager::isWithinCSGMinus2, this,
                           std::placeholders::_1);
}

Cell3DPosition ScaffoldManager::normalize(const Cell3DPosition& pos) const {
    return pos - seed;
}

Cell3DPosition ScaffoldManager::denormalize(const Cell3DPosition& pos) const {
    return pos + seed;
}

bool ScaffoldManager::isInScaffold(const Cell3DPosition& pos) const {
    return isOnBranch(XBranch, pos) or isOnBranch(YBranch, pos) or isOnBranch(ZBranch, pos)
        or (isOnBranch(OppXBranch, pos) or isOnBranch(OppYBranch, pos))
        or isOnBranch(RevZBranch, pos) or isOnBranch(RevZBranch, pos)
        or isOnBranch(LZBranch, pos) or isOnBranch(RZBranch, pos)
        or isHelperModule(pos);
}

bool ScaffoldManager::isHelperModule(const Cell3DPosition& pos) const {
    return (m_mod(pos[0], B) == 1 or m_mod(pos[0], B) == B - 1)
        and (m_mod(pos[1], B) == 1 or m_mod(pos[1], B) == B - 1)
        and m_mod(pos[2], B) == 0;
}

bool ScaffoldManager::isTileRoot(const Cell3DPosition& pos) const {
    return m_mod(pos[0], B) == 0 and m_mod(pos[1], B) == 0 and m_mod(pos[2], B) == 0;
}

bool ScaffoldManager::isOnBranch(BranchIndex bi, const Cell3DPosition& pos) const {
    switch(bi) {
        case XBranch:
            return m_mod(pos[1], B) == 0 and m_mod(pos[2], B) == 0
                and not isOnBranch(OppXBranch, pos);

        case YBranch:
            return m_mod(pos[0], B) == 0 and m_mod(pos[2], B) == 0
                and not isOnBranch(OppYBranch, pos);

        case ZBranch: {
            const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
            const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);

            return m_mod(x, B) == 0 and m_mod(y, B) == 0;
        }

        case RevZBranch: {
            const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
            const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
            const int z = pos[2];

            return m_mod(x, B) == m_mod(y, B) and m_mod(z, B) == m_mod(B - x, B);
        }

        case RZBranch: {
            const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
            const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
            const int z = pos[2];

            return (m_mod(y, B) + m_mod(z, B) == (int)B and m_mod(x, B) == 0);
        }

        case LZBranch: {
            const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
            const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
            const int z = pos[2];

            return (m_mod(x, B) + m_mod(z, B) == (int)B and m_mod(y, B) == 0);
        }

        case OppXBranch: {
            const Cell3DPosition& oppXTr = pos - m_mod(pos[0], B) * Cell3DPosition(1, 0, 0);
            return m_mod(pos[1], B) == 0 and m_mod(pos[2], B) == 0
                and (not isInsideFn(denormalize(oppXTr)));
        }

        case OppYBranch: {
            const Cell3DPosition& oppYTr = pos - m_mod(pos[1], B) * Cell3DPosition(0, 1, 0);

            return m_mod(pos[0], B) == 0 and m_mod(pos[2], B) == 0
                and (not isInsideFn(denormalize(oppYTr)));
        }

        default: break;
    }

    stringstream err;
    err << "isOnBranch(" << bi << ", " << pos << "): ";
    err << "invalid input branch index" << endl;
    throw ScaffoldManagerException(err.str());

    return false; // unreachable
}

Cell3DPosition ScaffoldManager::getBranchUnitOffset(int bi) const {
    switch(bi) {
        case ZBranch: return Cell3DPosition(0,0,1);
        case RevZBranch: return Cell3DPosition(-1,-1,1);
        case RZBranch: return Cell3DPosition(0,-1,1);
        case LZBranch: return Cell3DPosition(-1,0,1);
        case XBranch: return Cell3DPosition(1,0,0);
        case YBranch: return Cell3DPosition(0,1,0);
        case OppXBranch: return Cell3DPosition(-1,0,0);
        case OppYBranch: return Cell3DPosition(0,-1,0);
        default:
            stringstream err;
            err << "getBranchUnitOffset(" << bi << "): ";
            err << "invalid input branch index" << endl;
            throw ScaffoldManagerException(err.str());
    }

    return Cell3DPosition(0,0,0); // Unreachable
}

const Cell3DPosition
ScaffoldManager::getTileRootAtEndOfBranch(const Cell3DPosition& trRef,
                                          BranchIndex bi, bool outgoing) const {
    if (not isInScaffold(trRef)) return trRef;

    return outgoing ? trRef + B * getBranchUnitOffset(bi)
        : trRef - B * getBranchUnitOffset(bi);
}

bool ScaffoldManager::
hasIngoingBranch(const Cell3DPosition& pos, BranchIndex bi) const {
    // Invalid input
    if (not isTileRoot(pos)) return false;

    // If above sandbox, there are always all incident vertical branches
    if (bi < XBranch and pos[2] == 0) return true;

    // TR has incident branch if its parent tile TR exists and is in object
    const Cell3DPosition& trIVB = getTileRootAtEndOfBranch(pos, bi, false);

    // Make a distinction between X/Y and OppX/Y branches
    const Cell3DPosition bPos = pos + -1 * getBranchUnitOffset(bi);
    if ( (bi == OppXBranch and not isOnBranch(OppXBranch, bPos))
         or (bi == OppYBranch and not isOnBranch(OppYBranch, bPos)) )
        return false;

    return isInsideFn(denormalize(trIVB)) and resourcesForBranch(trIVB, bi) == (B - 1);
}

BranchIndex ScaffoldManager::getAlternateBranchIndex(BranchIndex bi) const {
    switch(bi) {
        case ZBranch: return RevZBranch;
        case RevZBranch: return ZBranch;
        case LZBranch: return RZBranch;
        case RZBranch: return LZBranch;
        default:
            stringstream err;
            err << "getAlternateBranchIndex(" << bi << "): ";
            err << "invalid input branch index" << endl;
            throw ScaffoldManagerException(err.str());
    }

    return N_BRANCHES; // unreachable
}

bool ScaffoldManager::shouldGrowBranch(const Cell3DPosition& pos,
                                       BranchIndex bi) const {
    return resourcesForBranch(pos, bi) > 0;
}

short ScaffoldManager::resourcesForBranch(const Cell3DPosition& pos, BranchIndex bi) const {
    if (not isTileRoot(pos)) return 0;

    if (bi < XBranch and pos[2] > B
        and not hasIngoingBranch(pos, getAlternateBranchIndex(bi)))
        return 0;

    for (int i = 0; i < B - 1; i++) {
        const Cell3DPosition bPos = pos + (i + 1) * getBranchUnitOffset(bi);

        if ( ((bi == OppXBranch and not isOnBranch(OppXBranch, bPos))
              or (bi == OppYBranch and not isOnBranch(OppYBranch, bPos)))
             or
             ((bi == XBranch and isOnBranch(OppXBranch, bPos))
              or (bi == YBranch and isOnBranch(OppYBranch, bPos))) )
            return 0;

        if (not isInScaffold(bPos) or not isInsideFn(denormalize(bPos))) {
            return i;
        }
    }

    return B - 1;
}

string ScaffoldManager::branch_to_string(BranchIndex bi) {
    switch(bi) {
        case RevZBranch: return "RevZBranch";
        case ZBranch: return "ZBranch";
        case RZBranch: return "RZBranch";
        case LZBranch: return "LZBranch";
        case XBranch: return "XBranch";
        case YBranch: return "YBranch";
        case OppXBranch: return "OppXBranch";
        case OppYBranch: return "OppYBranch";
        default: return "";
    }

    return "";
}

list<Cell3DPosition> ScaffoldManager::getAllSupportPositionsForPlane(int z) const {
    // Normalize to scaffold coordinate system
    int plane = z - seed[2];

    list<Cell3DPosition> supports = list<Cell3DPosition>();

    if (m_mod(plane, B) == 0) {
        const Cell3DPosition& glb = lattice->getGridLowerBounds(z);
        const Cell3DPosition& gub = lattice->getGridUpperBounds(z);

        Cell3DPosition pos;
        for (short iy = glb[1]; iy <= gub[1]; iy ++) {
            for (short ix = glb[0]; ix <= gub[0]; ix ++) {
                pos.set(ix, iy, z);
                Cell3DPosition tPos = normalize(pos);


                // cout << "pos: " << pos << endl;
                // cout << "tPos: " << tPos << endl;
                // cout << "isTileRoot(tpos): " << isTileRoot(tPos) << endl;
                // cout << "isInsideFn(pos): " << isInsideFn(pos) << endl;


                if (isTileRoot(tPos) and isInsideFn(pos)) {

                    // cout << "pos: " << pos << endl;
                    // cout << "tPos: " << tPos << endl;

                    for (int i = XBranch; i < N_BRANCHES; i++) {
                        BranchIndex bi = (BranchIndex)i;
                        short res = resourcesForBranch(tPos, bi);
                        Cell3DPosition sPos = pos + (res + 1) * getBranchUnitOffset(bi);

                        // cout << branch_to_string(bi) << "\t" << res << endl;
                        // cout << sPos << " : " << isInsideFn(sPos) << endl;

                        // Elongate all border horizontal branches by one module
                        //  branch is on the border if the next module along it is not in
                        //  the scaffold
                        if (not isInsideFn(sPos)) {
                            supports.push_back(sPos);
                            // lattice->highlightCell(sPos, CYAN);
                        }
                    }
                }
            }
        }
    }

    return supports;
}

bool ScaffoldManager::isWithinCSGMinus2(const Cell3DPosition& pos) const {
    if (not isInCSG(pos)) return false;

    // This is used to work with scaffolds that are smaller than the CSG they represent by
    //  two modules.

    for (int i = 0; i < N_BRANCHES; i++) {
        BranchIndex bi = (BranchIndex)i;
        // We do not need a distance 2 for vertical positions since that would leave too much
        // space at the top of the structure and prevent the attachment of the coating to
        // the scaffold
        const Cell3DPosition &pn = pos + (i < XBranch ? 1 : 2)*getBranchUnitOffset(bi);
        if (not isInCSG(pn)) {
            return false;
        }
    }

    return true;
}
