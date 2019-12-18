/**
 * @file   scaffoldingRuleMatcher.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jun 11 15:32:34 2019
 *
 * @brief
 *
 *
 */

#include "scaffoldingRuleMatcher.hpp"

#include "scaffoldingLocalRules.hpp" // for B2

#include "network.h"
#include "utils.h"
#include "catoms3DWorld.h"

using namespace MeshCoating;
using namespace BaseSimulator;
using namespace utils;
using namespace Catoms3D;

std::array<Cell3DPosition, N_COMPONENTS> ScaffoldingRuleMatcher::componentPosition = {
    Cell3DPosition(0, 0, 0), // R

    Cell3DPosition(1, 1, 0), // S_Z
    Cell3DPosition(-1, -1, 0), // S_RevZ
    Cell3DPosition(-1, 1, 0), // S_LZ
    Cell3DPosition(1, -1, 0), // S_RZ

    Cell3DPosition(1, 0, 0), // X1
    Cell3DPosition(2, 0, 0), // X2
    Cell3DPosition(3, 0, 0), // X3
    Cell3DPosition(4, 0, 0), // X4
    Cell3DPosition(5, 0, 0), // X5

    Cell3DPosition(0, 1, 0), // Y1
    Cell3DPosition(0, 2, 0), // Y2
    Cell3DPosition(0, 3, 0), // Y3
    Cell3DPosition(0, 4, 0), // Y4
    Cell3DPosition(0, 5, 0), // Y5

    Cell3DPosition(0, 0, 1), // Z1
    Cell3DPosition(0, 0, 2), // Z2
    Cell3DPosition(0, 0, 3), // Z3
    Cell3DPosition(0, 0, 4), // Z4
    Cell3DPosition(0, 0, 5), // Z5

    Cell3DPosition(-1, -1, 1), // RevZ1
    Cell3DPosition(-2, -2, 2), // RevZ2
    Cell3DPosition(-3, -3, 3), // RevZ3
    Cell3DPosition(-4, -4, 4), // RevZ4
    Cell3DPosition(-5, -5, 5), // RevZ5

    Cell3DPosition(-1, 0, 1), // LZ1
    Cell3DPosition(-2, 0, 2), // LZ2
    Cell3DPosition(-3, 0, 3), // LZ3
    Cell3DPosition(-4, 0, 4), // LZ4
    Cell3DPosition(-5, 0, 5), // LZ5

    Cell3DPosition(0, -1, 1), // RZ1
    Cell3DPosition(0, -2, 2), // RZ2
    Cell3DPosition(0, -3, 3), // RZ3
    Cell3DPosition(0, -4, 4), // RZ4
    Cell3DPosition(0, -5, 5), // RZ5

    Cell3DPosition(-1, 0, 0), // OPP_X1
    Cell3DPosition(-2, 0, 0), // OPP_X2
    Cell3DPosition(-3, 0, 0), // OPP_X3
    Cell3DPosition(-4, 0, 0), // OPP_X4
    Cell3DPosition(-5, 0, 0), // OPP_X5
    Cell3DPosition(0, -1, 0), // OPP_Y1
    Cell3DPosition(0, -2, 0), // OPP_Y2
    Cell3DPosition(0, -3, 0), // OPP_Y3
    Cell3DPosition(0, -4, 0), // OPP_Y4
    Cell3DPosition(0, -5, 0), // OPP_Y5

    Cell3DPosition(0, 0, -1), // OPP_Z1
    Cell3DPosition(0, 0, -2), // OPP_Z2
    Cell3DPosition(0, 0, -3), // OPP_Z3
    Cell3DPosition(0, 0, -4), // OPP_Z4
    Cell3DPosition(0, 0, -5), // OPP_Z5

    Cell3DPosition(1, 1, -1), // OPP_RevZ1
    Cell3DPosition(2, 2, -2), // OPP_RevZ2
    Cell3DPosition(3, 3, -3), // OPP_RevZ3
    Cell3DPosition(4, 4, -4), // OPP_RevZ4
    Cell3DPosition(5, 5, -5), // OPP_RevZ5

    Cell3DPosition(1, 0, -1), // OPP_LZ1
    Cell3DPosition(2, 0, -2), // OPP_LZ2
    Cell3DPosition(3, 0, -3), // OPP_LZ3
    Cell3DPosition(4, 0, -4), // OPP_LZ4
    Cell3DPosition(5, 0, -5), // OPP_LZ5

    Cell3DPosition(0, 1, -1), // OPP_RZ1
    Cell3DPosition(0, 2, -2), // OPP_RZ2
    Cell3DPosition(0, 3, -3), // OPP_RZ3
    Cell3DPosition(0, 4, -4), // OPP_RZ4
    Cell3DPosition(0, 5, -5), // OPP_RZ5

    // NEXT POSITIONS ARE __IN NEIGHBOR TILES__ BUT RELATIVE TO CURRENT TILE

    Cell3DPosition(-1, -1, B2 - 1), // RevZ_EPL
    Cell3DPosition(0, -1, B2 - 1), // RevZ_R_EPL

    Cell3DPosition(-(B2 - 1), -1, (B2 - 1)), // RZ_L_EPL
    Cell3DPosition(-(B2 - 2), -1, (B2 - 1)), // RZ_EPL
    Cell3DPosition(-(B2 - 2), 0, (B2 - 1)), // RZ_R_EPL

    Cell3DPosition(-(B2 - 2), -(B2 - 1), (B2 - 1)), // Z_R_EPL
    Cell3DPosition(-(B2 - 2), -(B2 - 2), (B2 - 1)), // Z_EPL
    Cell3DPosition(-(B2 - 1), -(B2 - 2), (B2 - 1)), // Z_L_EPL

    Cell3DPosition(0, -(B2 - 2), (B2 - 1)), // LZ_R_EPL
    Cell3DPosition(-1, -(B2 - 2), (B2 - 1)), // LZ_EPL
    Cell3DPosition(-1, -(B2 - 1), (B2 - 1)), // LZ_L_EPL

    Cell3DPosition(-1, 0, B2 - 1), // RevZ_L_EPL

    Cell3DPosition((B2 - 2), -1, 0), // OPP_X_R_EPL
    Cell3DPosition(-1, (B2 - 2), 0), // OPP_Y_L_EPL
    Cell3DPosition(1, (B2 - 2), 0), // OPP_Y_R_EPL
    Cell3DPosition(-(B2 - 2), -1, 0), // X_L_EPL
    Cell3DPosition(-(B2 - 2), 1, 0), // X_R_EPL
    Cell3DPosition(1, -(B2 - 2), 0), // Y_L_EPL
    Cell3DPosition(-1, -(B2 - 2), 0), // Y_R_EPL
    Cell3DPosition((B2 - 2), 1, 0), // OPP_X_L_EPL
};

string ScaffoldingRuleMatcher::component_to_string(ScafComponent comp) {
    switch(comp) {
        case R: return "R";
        case S_Z: return "S_Z";
        case S_RevZ: return "S_RevZ";
        case S_LZ: return "S_LZ";
        case S_RZ: return "S_RZ";
        case X_1: return "X_1";
        case X_2: return "X_2";
        case X_3: return "X_3";
        case X_4: return "X_4";
        case X_5: return "X_5";
        case Y_1: return "Y_1";
        case Y_2: return "Y_2";
        case Y_3: return "Y_3";
        case Y_4: return "Y_4";
        case Y_5: return "Y_5";
        case Z_1: return "Z_1";
        case Z_2: return "Z_2";
        case Z_3: return "Z_3";
        case Z_4: return "Z_4";
        case Z_5: return "Z_5";
        case RevZ_1: return "RevZ_1";
        case RevZ_2: return "RevZ_2";
        case RevZ_3: return "RevZ_3";
        case RevZ_4: return "RevZ_4";
        case RevZ_5: return "RevZ_5";
        case LZ_1: return "LZ_1";
        case LZ_2: return "LZ_2";
        case LZ_3: return "LZ_3";
        case LZ_4: return "LZ_4";
        case LZ_5: return "LZ_5";
        case RZ_1: return "RZ_1";
        case RZ_2: return "RZ_2";
        case RZ_3: return "RZ_3";
        case RZ_4: return "RZ_4";
        case RZ_5: return "RZ_5";
        case OPP_X1: return "OPP_X1";
        case OPP_X2: return "OPP_X2";
        case OPP_X3: return "OPP_X3";
        case OPP_X4: return "OPP_X4";
        case OPP_X5: return "OPP_X5";
        case OPP_Y1: return "OPP_Y1";
        case OPP_Y2: return "OPP_Y2";
        case OPP_Y3: return "OPP_Y3";
        case OPP_Y4: return "OPP_Y4";
        case OPP_Y5: return "OPP_Y5";
        case RevZ_EPL: return "RevZ_EPL";
        case RevZ_R_EPL: return "RevZ_R_EPL";
        case RZ_L_EPL: return "RZ_L_EPL";
        case RZ_EPL: return "RZ_EPL";
        case RZ_R_EPL: return "RZ_R_EPL";
        case Z_R_EPL: return "Z_R_EPL";
        case Z_EPL: return "Z_EPL";
        case Z_L_EPL: return "Z_L_EPL";
        case LZ_R_EPL: return "LZ_R_EPL";
        case LZ_EPL: return "LZ_EPL";
        case LZ_L_EPL: return "LZ_L_EPL";
        case RevZ_L_EPL: return "RevZ_L_EPL";
        case OPP_X_R_EPL: return "OPP_X_R_EPL";
        case OPP_Y_L_EPL: return "OPP_Y_L_EPL";
        case OPP_Y_R_EPL: return "OPP_Y_R_EPL";
        case X_L_EPL: return "X_L_EPL";
        case X_R_EPL: return "X_R_EPL";
        case Y_L_EPL: return "Y_L_EPL";
        case Y_R_EPL: return "Y_R_EPL";
        case OPP_X_L_EPL: return "OPP_X_L_EPL";
        case OPP_Z1: return "OPP_Z1";
        case OPP_Z2: return "OPP_Z2";
        case OPP_Z3: return "OPP_Z3";
        case OPP_Z4: return "OPP_Z4";
        case OPP_Z5: return "OPP_Z5";
        case OPP_RevZ1: return "OPP_RevZ1";
        case OPP_RevZ2: return "OPP_RevZ2";
        case OPP_RevZ3: return "OPP_RevZ3";
        case OPP_RevZ4: return "OPP_RevZ4";
        case OPP_RevZ5: return "OPP_RevZ5";
        case OPP_LZ1: return "OPP_LZ1";
        case OPP_LZ2: return "OPP_LZ2";
        case OPP_LZ3: return "OPP_LZ3";
        case OPP_LZ4: return "OPP_LZ4";
        case OPP_LZ5: return "OPP_LZ5";
        case OPP_RZ1: return "OPP_RZ1";
        case OPP_RZ2: return "OPP_RZ2";
        case OPP_RZ3: return "OPP_RZ3";
        case OPP_RZ4: return "OPP_RZ4";
        case OPP_RZ5: return "OPP_RZ5";

        case N_COMPONENTS: return "N_COMPONENTS";
    }

    return "UNDEFINED"; // UNREACHABLE
}

Cell3DPosition ScaffoldingRuleMatcher::getPositionForComponent(ScafComponent comp) {
    return comp < N_COMPONENTS ? componentPosition[comp] : Cell3DPosition();
}

int ScaffoldingRuleMatcher::getComponentForPosition(const Cell3DPosition& pos) {
    for (int i = 0; i <= N_COMPONENTS; i++)
        if (componentPosition[i] == pos) return i;

    return -1;
}

ScafComponent ScaffoldingRuleMatcher::getDefaultEPLComponentForBranch(BranchIndex bi) {
    switch(bi) {
        case ZBranch: return Z_EPL;
        case RevZBranch: return RevZ_EPL;
        case RZBranch: return RZ_EPL;
        case LZBranch: return LZ_EPL;

        // case XBranch: ;
        // case YBranch:
        // case XOppBranch:
        // case YOppBranch:

        default: break;
    }

    cerr << "getDefaultEPLComponentForBranch: invalid input: " << bi << endl;
    VS_ASSERT(false);

    return R; // unreachable
}

ScafComponent ScaffoldingRuleMatcher::getTargetEPLComponentForBranch(BranchIndex bi) {
    switch(bi) {
        case ZBranch: return RevZ_EPL;
        case RevZBranch: return Z_EPL;
        case RZBranch: return LZ_EPL;
        case LZBranch: return RZ_EPL;

        default: break;
    }

    cerr << "getTargetEPLComponentForBranch: invalid input: " << bi << endl;
    VS_ASSERT(false);

    return R; // unreachable
}

void ScaffoldingRuleMatcher::printDebugInfo(const Cell3DPosition& pos) const {
  cout << "--- DEBUG INFO: " << pos << endl;
  cout << "isTileRoot(pos): " << isTileRoot(pos) << endl;
  cout << "isOnXBranch(pos): " << isOnXBranch(pos) << endl;
  cout << "isOnOppXBranch(pos): " << isOnOppXBranch(pos) << endl;
  cout << "isOnYBranch(pos): " << isOnYBranch(pos) << endl;
  cout << "isOnOppYBranch(pos): " << isOnOppYBranch(pos) << endl;
  cout << "isOnZBranch(pos): " << isOnZBranch(pos) << endl;
  cout << "isOnRevZBranch(pos): " << isOnRevZBranch(pos) << endl;
  cout << "isOnRZBranch(pos): " << isOnRZBranch(pos) << endl;
  cout << "isOnLZBranch(pos): " << isOnLZBranch(pos) << endl;
  cout << "--- END DEBUG INFO ---" << endl << endl;
}

bool ScaffoldingRuleMatcher::isInGrid(const Cell3DPosition& pos) const {
    // cout << X_MAX << ", " << Y_MAX << ", " << Z_MAX << endl;
    return (isInRange(pos[0], 0 - pos[2]/ 2, X_MAX - pos[2] / 2)
            and isInRange(pos[1], 0 - pos[2] / 2, Y_MAX - pos[2] / 2)
            and isInRange(pos[2], 0, Z_MAX))
        or (isSupportModule(pos)
            and isInRange(pos[0], -1 - pos[2]/ 2, X_MAX - pos[2] / 2 - 1)
            and isInRange(pos[1], -1 - pos[2] / 2, Y_MAX - pos[2] / 2 - 1)
            and isInRange(pos[2], -1, Z_MAX - 1));
}

bool ScaffoldingRuleMatcher::isInSandbox(const Cell3DPosition& pos) const {
    return isInMesh(pos) and pos[2] < 0;
}

bool ScaffoldingRuleMatcher::isInMesh(const Cell3DPosition& pos) const {
    return isOnXBranch(pos) or isOnYBranch(pos) or isOnZBranch(pos)
        or (isOnOppXBranch(pos) or isOnOppYBranch(pos))
        or isOnRevZBranch(pos) or isOnRZBranch(pos)
        or isOnLZBranch(pos)
        or isSupportModule(pos);
}

bool ScaffoldingRuleMatcher::isInCSGMeshOrSandbox(const Cell3DPosition& pos) const {
    return (isInCSGFn(pos) and isInMesh(pos)) or isInSandbox(pos);
}

bool ScaffoldingRuleMatcher::isOnBranch(BranchIndex bi, const Cell3DPosition& pos) const {
    switch(bi) {
        case XBranch: return isOnXBranch(pos);
        case YBranch: return isOnYBranch(pos);
        case ZBranch: return isOnZBranch(pos);
        case RevZBranch: return isOnRevZBranch(pos);
        case RZBranch: return isOnRZBranch(pos);
        case LZBranch: return isOnLZBranch(pos);

        case OppXBranch: return isOnOppXBranch(pos);
        case OppYBranch: return isOnOppYBranch(pos);
        // case OppZBranch: return isOnOppZBranch(pos);
        // case OppRevZBranch: return isOnOppRevZBranch(pos);
        // case OppRZBranch: return isOnOppRZBranch(pos);
        // case OppLZBranch: return isOnOppLZBranch(pos);

        default: break;
    }

    cerr << "isOnBranch: invalid input: " << bi << endl;
    VS_ASSERT(false);

    return false; // unreachable
}

bool ScaffoldingRuleMatcher::isOnXBranch(const Cell3DPosition& pos) const {
    return m_mod(pos[1], B) == 0 and m_mod(pos[2], B) == 0
        and not isOnOppXBranch(pos);
}

bool ScaffoldingRuleMatcher::isOnOppXBranch(const Cell3DPosition& pos) const {
    const Cell3DPosition& oppXTr = pos - m_mod(pos[0], B) * Cell3DPosition(1, 0, 0);
    return m_mod(pos[1], B) == 0 and m_mod(pos[2], B) == 0
        and (not isInsideFn(oppXTr)
             // or (oppXTr[0] < getSeedForCSGLayer(pos[2] / B)[0])
            );
}

bool ScaffoldingRuleMatcher::isOnYBranch(const Cell3DPosition& pos) const {
    return m_mod(pos[0], B) == 0 and m_mod(pos[2], B) == 0
        and not isOnOppYBranch(pos);
}

bool ScaffoldingRuleMatcher::isOnOppYBranch(const Cell3DPosition& pos) const {
    const Cell3DPosition& oppYTr = pos - m_mod(pos[1], B) * Cell3DPosition(0, 1, 0);
    // const Cell3DPosition& oppYTr = pos - m_mod(pos[1], B) * Cell3DPosition(0, 1, 0);

    return m_mod(pos[0], B) == 0 and m_mod(pos[2], B) == 0
        and (not isInsideFn(oppYTr)
             // or (oppYTr[1] < getSeedForCSGLayer(pos[2] / B)[1])
            );
}

bool ScaffoldingRuleMatcher::isOnZBranch(const Cell3DPosition& pos) const {
    const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
    const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);

    return m_mod(x, B) == 0 and m_mod(y, B) == 0;
}

bool ScaffoldingRuleMatcher::isOnRevZBranch(const Cell3DPosition& pos) const {
    const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
    const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
    const int z = pos[2];

    return m_mod(x, B) == m_mod(y, B) and m_mod(z, B) == m_mod(B - x, B);
}

bool ScaffoldingRuleMatcher::isOnLZBranch(const Cell3DPosition& pos) const {
    const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
    const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
    const int z = pos[2];

    return (m_mod(x, B) + m_mod(z, B) == (int)B and m_mod(y, B) == 0);
}

bool ScaffoldingRuleMatcher::isOnRZBranch(const Cell3DPosition& pos) const {
    const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
    const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
    const int z = pos[2];

    return (m_mod(y, B) + m_mod(z, B) == (int)B and m_mod(x, B) == 0);
}


bool ScaffoldingRuleMatcher::isVerticalBranchTip(const Cell3DPosition& pos) const {
    return isNFromVerticalBranchTip(pos, 0);
}

bool ScaffoldingRuleMatcher::isNFromVerticalBranchTip(const Cell3DPosition& pos, int N) const {
    return (isOnZBranch(pos) or isOnRevZBranch(pos)
            or isOnLZBranch(pos) or isOnRZBranch(pos)) and m_mod(pos[2], B) == (B - N - 1);
}

bool ScaffoldingRuleMatcher::isEPLPivotModule(const Cell3DPosition& pos) const {
    return isNFromVerticalBranchTip(pos, 1);
}

bool ScaffoldingRuleMatcher::isBranchModule(const Cell3DPosition& pos) const {
    return (not isTileRoot(pos)) and (isOnZBranch(pos) or isOnRevZBranch(pos)
                                 or isOnLZBranch(pos) or isOnRZBranch(pos)
                                 or isOnXBranch(pos) or isOnYBranch(pos));
}

bool ScaffoldingRuleMatcher::isZBranchModule(const Cell3DPosition& pos) const {
    return (not isTileRoot(pos)) and (isOnZBranch(pos) or isOnRevZBranch(pos)
            or isOnLZBranch(pos) or isOnRZBranch(pos));
}

bool ScaffoldingRuleMatcher::isSupportModule(const Cell3DPosition& pos) const {
    return (m_mod(pos[0], B) == 1 or m_mod(pos[0], B) == B - 1)
        and (m_mod(pos[1], B) == 1 or m_mod(pos[1], B) == B - 1)
        and m_mod(pos[2], B) == 0;
}

BranchIndex
ScaffoldingRuleMatcher::getBranchIndexForNonRootPosition(const Cell3DPosition& pos) const{
    if (isSupportModule(pos)) {
        if (isTileRoot(pos - getPositionForComponent(S_RevZ)))
            return RevZBranch;
        else if (isTileRoot(pos - getPositionForComponent(S_Z)))
            return ZBranch;
        else if (isTileRoot(pos - getPositionForComponent(S_RZ)))
            return RZBranch;
        else if (isTileRoot(pos - getPositionForComponent(S_LZ)))
            return LZBranch;
    }

    VS_ASSERT_MSG(isInMesh(pos), "attempting to get branch index of position outside of mesh");
    VS_ASSERT_MSG(not isTileRoot(pos), "attempting to get branch index of tile root position:");

    if (isOnXBranch(pos)) return XBranch;
    if (isOnYBranch(pos)) return YBranch;
    if (isOnZBranch(pos)) return ZBranch;
    if (isOnRevZBranch(pos)) return RevZBranch;
    if (isOnLZBranch(pos)) return LZBranch;
    if (isOnRZBranch(pos)) return RZBranch;
    if (isOnOppXBranch(pos)) return OppXBranch;
    if (isOnOppYBranch(pos)) return OppYBranch;

    VS_ASSERT(false); // Unreachable

    return N_BRANCHES;
}

bool ScaffoldingRuleMatcher::shouldGrowBranch(const Cell3DPosition& pos,
                                              BranchIndex bi) const {
    return resourcesForBranch(pos, bi) > 0;
}

short ScaffoldingRuleMatcher::resourcesForBranch(const Cell3DPosition& pos,
                                                 BranchIndex bi) const {
    if (not isTileRoot(pos)) return 0;

    // FIXME: Do not consider a vertical branch for growth if the feeding branch does not exist
    if (bi < XBranch and pos[2] > B // NOTE: Sandbox gets counted as no incident branch
        and not hasIncidentBranch(pos, getAlternateBranchIndex(bi)))
        return 0;

    for (int i = 0; i < B - 1; i++) {
        const Cell3DPosition bPos = pos + (i + 1) * getBranchUnitOffset(bi);

        if ( ((bi == OppXBranch and not isOnOppXBranch(bPos))
              or (bi == OppYBranch and not isOnOppYBranch(bPos)))
             or
             ((bi == XBranch and isOnOppXBranch(bPos))
              or (bi == YBranch and isOnOppYBranch(bPos))) )
            return 0;

        if (not isInMesh(bPos) or not isInCSGFn(bPos)) {
            // cout << branch_to_string(bi) << ": " << i << endl;
            return i;
        }
    }

    // cout << branch_to_string(bi) << ": " << B - 1 << endl;
    return B - 1;
}

Cell3DPosition ScaffoldingRuleMatcher::getBranchUnitOffset(int bi) const {
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
            cerr << "bi: " << bi << endl;
            VS_ASSERT_MSG(false, "invalid branch index");
    }

    return Cell3DPosition(0,0,0); // Unreachable
}

Cell3DPosition ScaffoldingRuleMatcher::getBranchUnitOffset(const Cell3DPosition& pos) const {
    return getBranchUnitOffset(getBranchIndexForNonRootPosition(pos));
}

bool ScaffoldingRuleMatcher::isTileRoot(const Cell3DPosition& pos) const {
    return m_mod(pos[0], B) == 0 and m_mod(pos[1], B) == 0 and m_mod(pos[2], B) == 0;
}

short ScaffoldingRuleMatcher::determineBranchForPosition(const Cell3DPosition& pos) const {

    if (isInCSGMeshOrSandbox(pos) and not isTileRoot(pos)) {
        if (isOnZBranch(pos)) return ZBranch;
        if (isOnRevZBranch(pos)) return RevZBranch;
        if (isOnLZBranch(pos)) return LZBranch;
        if (isOnRZBranch(pos)) return RZBranch;
        if (isOnXBranch(pos)) return XBranch;
        if (isOnYBranch(pos)) return YBranch;
        if (isOnOppXBranch(pos)) return OppXBranch;
        if (isOnOppXBranch(pos)) return OppYBranch;
    }

    return -1;
}

const Cell3DPosition
ScaffoldingRuleMatcher::getNearestTileRootPosition(const Cell3DPosition& pos) const {
    int trX;
    if (m_mod(pos[0], B) >= B / 2) // over
        trX = pos[0] + (B - m_mod(pos[0], B));
    else // under
        trX = pos[0] - m_mod(pos[0], B);

    int trY;
    if (m_mod(pos[1], B) >= B / 2) // over
        trY = pos[1] + (B - m_mod(pos[1], B));
    else // under
        trY = pos[1] - m_mod(pos[1], B);

    int trZ;
    if (m_mod(pos[2], B) >= B / 2) // over
        trZ = pos[2] + (B - m_mod(pos[2], B));
    else // under
        trZ = pos[2] - m_mod(pos[2], B);

    const Cell3DPosition nearestTileRootPos = Cell3DPosition(trX, trY, trZ);

    VS_ASSERT_MSG(isTileRoot(nearestTileRootPos), "computed position is not tile root!");

    return nearestTileRootPos;
}

const Cell3DPosition
ScaffoldingRuleMatcher::getTileRootPositionForMeshPosition(const Cell3DPosition& pos) const {
    // cout << "pos: " << pos << endl;
    if (isSupportModule(pos)) {
        if (isTileRoot(pos - getPositionForComponent(S_RevZ)))
            return pos - getPositionForComponent(S_RevZ);
        else if (isTileRoot(pos - getPositionForComponent(S_Z)))
            return pos - getPositionForComponent(S_Z);
        else if (isTileRoot(pos - getPositionForComponent(S_RZ)))
            return pos - getPositionForComponent(S_RZ);
        else if (isTileRoot(pos - getPositionForComponent(S_LZ)))
            return pos - getPositionForComponent(S_LZ);
    } else if (isTileRoot(pos)) return pos;

    BranchIndex bi = getBranchIndexForNonRootPosition(pos);

    // cout << "bi: " << branch_to_string(bi) << endl;

    bool iis = isInSandbox(pos);
    const Cell3DPosition& mult = Cell3DPosition(
        iis ? m_mod(pos[0] + B, B) : m_mod(pos[0], B),
        iis ? m_mod(pos[1] + B, B) : m_mod(pos[1], B),
        iis ? m_mod(pos[2] + B, B) : m_mod(pos[2], B));

    // cout << "mult: " << mult << endl;
    int rank = std::max(mult[0], max(mult[1], mult[2]));
    // cout << "rank: " << rank << endl;

    const Cell3DPosition& rPos = bi < OppXBranch ?
        pos - rank * getBranchUnitOffset(bi) : pos + rank * getBranchUnitOffset(bi);

    // cout << "rPos: " << mult << endl;

    return rPos;
}

bool ScaffoldingRuleMatcher::isInTileWithRootAt(const Cell3DPosition& root,
                                                const Cell3DPosition& pos) const {
    return root == getTileRootPositionForMeshPosition(pos);
}

const Cell3DPosition
ScaffoldingRuleMatcher::getSupportPositionForPosition(const Cell3DPosition& pos) const {
    if (isOnZBranch(pos)) return Cell3DPosition(-1, -1, 0);
    if (isOnRevZBranch(pos)) return Cell3DPosition(1, 1, 0);
    if (isOnRZBranch(pos)) return Cell3DPosition(-1, 1, 0);
    if (isOnLZBranch(pos)) return Cell3DPosition(1, -1, 0);

    return Cell3DPosition();
}

AgentRole ScaffoldingRuleMatcher::getRoleForPosition(const Cell3DPosition& pos) const {
    if (not isInMesh(pos)) return AgentRole::FreeAgent;
    else {
        if (isTileRoot(pos)) return AgentRole::Coordinator;
        else if (isVerticalBranchTip(pos)) return AgentRole::ActiveBeamTip;
        else if (isSupportModule(pos)) return AgentRole::Support;
        else return AgentRole::PassiveBeam;
    }
}

const Cell3DPosition ScaffoldingRuleMatcher::getPositionForScafComponent(ScafComponent mc) {
    switch(mc) {
        case R: return Cell3DPosition(0,0,0);
        case S_Z: return Cell3DPosition(1,1,0);
        case S_RevZ: return Cell3DPosition(-1,-1,0);
        case S_LZ: return Cell3DPosition(-1,1,0);
        case S_RZ: return Cell3DPosition(1,-1,0);

        case X_1: case X_2: case X_3: case X_4: case X_5:
            return Cell3DPosition(1 * (mc - X_1 + 1), 0, 0);

        case Y_1: case Y_2: case Y_3: case Y_4: case Y_5:
            return Cell3DPosition(0, 1 * (mc - Y_1 + 1), 0);

        case Z_1: case Z_2: case Z_3: case Z_4: case Z_5:
            return Cell3DPosition(0, 0, 1 * (mc - Z_1 + 1));

        case RevZ_1: case RevZ_2: case RevZ_3: case RevZ_4: case RevZ_5:
            return Cell3DPosition(-1 * (mc - RevZ_1 + 1), -1 * (mc - RevZ_1 + 1),
                                  1 * (mc - RevZ_1 + 1));

        case LZ_1: case LZ_2: case LZ_3: case LZ_4: case LZ_5:
            return Cell3DPosition(-1 * (mc - LZ_1 + 1), 0, 1 * (mc - LZ_1 + 1));

        case RZ_1: case RZ_2: case RZ_3: case RZ_4: case RZ_5:
            return Cell3DPosition(0, -1 * (mc - RZ_1 + 1), 1 * (mc - RZ_1 + 1));

        // case OPP
        case OPP_X1: case OPP_X2: case OPP_X3: case OPP_X4: case OPP_X5:
            return Cell3DPosition(-1 * (mc - OPP_X1 + 1), 0, 0);

        case OPP_Y1: case OPP_Y2: case OPP_Y3: case OPP_Y4: case OPP_Y5:
            return Cell3DPosition(0, -1 * (mc - OPP_Y1 + 1), 0);

        case OPP_Z1: case OPP_Z2: case OPP_Z3: case OPP_Z4: case OPP_Z5:
            return Cell3DPosition(0, 0, -1 * (mc - OPP_Z1 + 1));

        case OPP_RevZ1: case OPP_RevZ2: case OPP_RevZ3: case OPP_RevZ4: case OPP_RevZ5:
            return Cell3DPosition(1 * (mc - OPP_RevZ1 + 1), 1 * (mc - OPP_RevZ1 + 1),
                                  -1 * (mc - OPP_RevZ1 + 1));

        case OPP_LZ1: case OPP_LZ2: case OPP_LZ3: case OPP_LZ4: case OPP_LZ5:
            return Cell3DPosition(1 * (mc - OPP_LZ1 + 1), 0, -1 * (mc - OPP_LZ1 + 1));

        case OPP_RZ1: case OPP_RZ2: case OPP_RZ3: case OPP_RZ4: case OPP_RZ5:
            return Cell3DPosition(0, 1 * (mc - OPP_RZ1 + 1), -1 * (mc - OPP_RZ1 + 1));

        // case EPLs
        case RevZ_EPL: return Cell3DPosition(-1,-1,-1);
        case RevZ_R_EPL: return Cell3DPosition(0,-1,-1);
        case RZ_L_EPL: return Cell3DPosition(1,-1,-1);
        case RZ_EPL: return Cell3DPosition(2,-1,-1);
        case RZ_R_EPL: return Cell3DPosition(2,0,-1);
        case Z_R_EPL: return Cell3DPosition(2,1,-1);
        case Z_EPL: return Cell3DPosition(2,2,-1);
        case Z_L_EPL: return Cell3DPosition(1,2,-1);
        case LZ_R_EPL: return Cell3DPosition(0,2,-1);
        case LZ_EPL: return Cell3DPosition(-1,2,-1);
        case LZ_L_EPL: return Cell3DPosition(-1,1,-1);
        case RevZ_L_EPL: return Cell3DPosition(-1,0,-1);

        case OPP_X_R_EPL: return Cell3DPosition(-2, -1, 0);
        case OPP_Y_L_EPL: return Cell3DPosition(-1, -2, 0);
        case OPP_Y_R_EPL: return Cell3DPosition(1, -2, 0);
        case X_L_EPL: return Cell3DPosition(2, -1, 0);
        case X_R_EPL: return Cell3DPosition(2, 1, 0);
        case Y_L_EPL: return Cell3DPosition(1, 2, 0);
        case Y_R_EPL: return Cell3DPosition(-1, 2, 0);
        case OPP_X_L_EPL: return Cell3DPosition(-2, 1, 0);

        case N_COMPONENTS: return Cell3DPosition();
    }

    return componentPosition[mc];
}

int ScaffoldingRuleMatcher::getBranchIndexForScafComponent(ScafComponent mc) {
    switch(mc) {
        case X_1: case X_2: case X_3: case X_4: case X_5:
            return XBranch;

        case Y_1: case Y_2: case Y_3: case Y_4: case Y_5:
            return YBranch;

        case Z_1: case Z_2: case Z_3: case Z_4: case Z_5:
            return ZBranch;

        case RevZ_1: case RevZ_2: case RevZ_3: case RevZ_4: case RevZ_5:
            return RevZBranch;

        case LZ_1: case LZ_2: case LZ_3: case LZ_4: case LZ_5:
            return LZBranch;

        case RZ_1: case RZ_2: case RZ_3: case RZ_4: case RZ_5:
            return RZBranch;

        case OPP_X1: case OPP_X2: case OPP_X3: case OPP_X4: case OPP_X5:
            return OppXBranch;

        case OPP_Y1: case OPP_Y2: case OPP_Y3: case OPP_Y4: case OPP_Y5:
            return OppYBranch;

        default: break;
    }

    return -1;
}

const Cell3DPosition
ScaffoldingRuleMatcher::getPositionForChildTileScafComponent(ScafComponent mc) const {
    switch(mc) {
        // case EPLs
        // case RevZ_EPL: return Cell3DPosition(-1,-1,-1);
        // case RevZ_R_EPL: return Cell3DPosition(0,-1,-1);
        // case RZ_L_EPL: return Cell3DPosition(1,-1,-1);
        // case RZ_EPL: return Cell3DPosition(2,-1,-1);
        case RZ_R_EPL:
            return getPositionForScafComponent(mc) + Cell3DPosition(-(short)B,0,B);
        case Z_R_EPL:
            return getPositionForScafComponent(mc) + Cell3DPosition(-(short)B,-(short)B,B);
        // case Z_EPL: return Cell3DPosition(2,2,-1);
        // case Z_L_EPL: return Cell3DPosition(1,2,-1);
        case LZ_R_EPL:
            return getPositionForScafComponent(mc) + Cell3DPosition(0,-(short)B,B);
        // case LZ_EPL: return Cell3DPosition(-1,2,-1);
        // case LZ_L_EPL: return Cell3DPosition(-1,1,-1);
        // case RevZ_L_EPL: return Cell3DPosition(-1,0,-1);
        default: VS_ASSERT_MSG(false, "Child tile mesh component must be an EPL");
    }

    return Cell3DPosition(); // unreachable
}

const Color& ScaffoldingRuleMatcher::getColorForPosition(const Cell3DPosition& pos) const {
    switch(getRoleForPosition(pos)) {
        case Coordinator: return GREEN;
        case Support: return YELLOW;
        case FreeAgent: return ORANGE;
        case ActiveBeamTip: return RED;
        case PassiveBeam: return BLUE;
    }

    return GREY;
}

Cell3DPosition ScaffoldingRuleMatcher::getPositionOfBranchTipUnder(BranchIndex bi) {
    switch(bi) {
        case ZBranch: return Cell3DPosition(1, 1, -1);
        case RevZBranch: return Cell3DPosition(0, 0, -1);
        case LZBranch: return Cell3DPosition(0, 1, -1);
        case RZBranch: return Cell3DPosition(1, 0, -1);
        default:
            cerr << "getIndexOfBranchTipUnder(" << bi << ") invalid input. " << endl;
            VS_ASSERT(false);
    }

    return Cell3DPosition(); // unreachable
}

BranchIndex ScaffoldingRuleMatcher::getAlternateBranchIndex(BranchIndex bi) const {
    switch(bi) {
        case ZBranch: return RevZBranch;
        case RevZBranch: return ZBranch;
        case LZBranch: return RZBranch;
        case RZBranch: return LZBranch;
        default:
            cerr << "getAlternateBranchIndex(" << bi << ") invalid input. " << endl;
            VS_ASSERT(false);
    }

    return N_BRANCHES; // unreachable
}


string ScaffoldingRuleMatcher::roleToString(AgentRole ar) {
    switch(ar) {
        case FreeAgent: return "FreeAgent";
        case Coordinator: return "Coordinator";
        case PassiveBeam: return "Beam";
        case ActiveBeamTip: return "Relay";
        case Support: return "Relay";
    }

    return "";
}

string ScaffoldingRuleMatcher::branch_to_string(BranchIndex bi) {
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

BranchIndex ScaffoldingRuleMatcher::getBranchForEPL(ScafComponent epl) {
    switch(epl) {
        case RevZ_EPL: return RevZBranch;
        case RevZ_R_EPL: return RevZBranch;
        case RZ_L_EPL: return RZBranch;
        case RZ_EPL: return RZBranch;
        case RZ_R_EPL: return RZBranch;
        case Z_R_EPL: return ZBranch;
        case Z_EPL: return ZBranch;
        case Z_L_EPL: return ZBranch;
        case LZ_R_EPL: return LZBranch;
        case LZ_EPL: return LZBranch;
        case LZ_L_EPL: return LZBranch;
        case RevZ_L_EPL: return RevZBranch;

        case OPP_X_R_EPL: return OppXBranch;
        case OPP_Y_L_EPL: return OppYBranch;
        case OPP_Y_R_EPL: return OppYBranch;
        case X_L_EPL: return XBranch;
        case X_R_EPL: return XBranch;
        case Y_L_EPL: return YBranch;
        case Y_R_EPL: return YBranch;
        case OPP_X_L_EPL: return OppXBranch;

        default: VS_ASSERT_MSG(false, "getBranchForEPL: input epl is not an EPL");
    }

    return N_BRANCHES;
}

const Cell3DPosition ScaffoldingRuleMatcher::getTargetEPLPositionForBranch(BranchIndex bi) {
    switch (bi) {
        case RevZBranch: return Cell3DPosition(-(B2 - 2), -(B2 - 2), (B2 - 1));
        case RZBranch: return Cell3DPosition(-1, -(B2 - 2), (B2 - 1));
        case ZBranch: return Cell3DPosition(-1, -1, (B2 - 1));
        case LZBranch: return Cell3DPosition(-(B2 - 2), -1, (B2 - 1));

        default:
            cerr << "getTargetEPLPositionForBranch(" << bi << ")" << endl;
            VS_ASSERT_MSG(false, "getTargetEPLPositionForBranch: input is not a valid branch");
    }

    return Cell3DPosition(); // unreachable
}

const Cell3DPosition
ScaffoldingRuleMatcher::getTileRootAtEndOfBranch(const Cell3DPosition& trRef,
                                          BranchIndex bi,
                                          bool upward) const {
    if (not isInMesh(trRef)) return trRef;

    return upward ? trRef + B * getBranchUnitOffset(bi)
        : trRef - B * getBranchUnitOffset(bi);
}

short ScaffoldingRuleMatcher::
getNbIncidentVerticalBranches(const Cell3DPosition& pos) const {
    // Invalid input
    if (not isTileRoot(pos)) return -1;

    // Tile is right above sandbox
    if (pos[2] == 0) return 4;

    // Otherwise, count
    short count = 0;
    for (int bi = 0; bi < XBranch; bi++)
        if (hasIncidentBranch(pos, (BranchIndex)bi)) ++count;

    return count;
}


short ScaffoldingRuleMatcher::
getNbIncidentBranches(const Cell3DPosition& pos) const {
    // Invalid input
    if (not isTileRoot(pos)) return -1;

    // Otherwise, count
    short count = 0;
    for (int bi = 0; bi < N_BRANCHES; bi++) {
        // ignore sandbox
        if (pos[2] == 0 and bi < XBranch) continue;

        if (hasIncidentBranch(pos, (BranchIndex)bi)) ++count;
    }

    return count;
}

bool ScaffoldingRuleMatcher::
hasIncidentBranch(const Cell3DPosition& pos, BranchIndex bi) const {
    // Invalid input
    if (not isTileRoot(pos)) return false;

    // If above sandbox, there are always all incident branches vertical
    if (bi < XBranch and pos[2] == 0) return true;

    // TR has incident branch if its parent tile TR exists and is in object
    const Cell3DPosition& trIVB = getTileRootAtEndOfBranch(pos, bi, false);

    // Make a distinction between X/Y and OppX/Y branches
    const Cell3DPosition bPos = pos + -1 * getBranchUnitOffset(bi);
    if ( (bi == OppXBranch and not isOnOppXBranch(bPos))
         or (bi == OppYBranch and not isOnOppYBranch(bPos)) )
        return false;

    return isInCSGFn(trIVB) and resourcesForBranch(trIVB, bi) == (B - 1);
}

const Cell3DPosition
ScaffoldingRuleMatcher::getEntryPointPosition(const Cell3DPosition& cPos,
                                              ScafComponent epl) const {
    return getEntryPointRelativePos(epl) + cPos;
}


bool ScaffoldingRuleMatcher::isOnHorizontalEPL(const Cell3DPosition& pos) const {
    const Cell3DPosition& nearestTR = getNearestTileRootPosition(pos);

    for (int i = OPP_X_R_EPL - RevZ_EPL; i <= OPP_X_L_EPL - RevZ_EPL; i++) {
        const Cell3DPosition& ep = getEntryPointRelativePos(i);
        if (pos == ep + nearestTR) return true;
    }

    return false;
}

bool ScaffoldingRuleMatcher::isOnEntryPoint(const Cell3DPosition& pos) const {
    const Cell3DPosition& nearestTR = getNearestTileRootPosition(pos);

    for (int i = 0; i < 20; i++) {
        const Cell3DPosition& ep = getEntryPointRelativePos(i);
        if (pos == ep + nearestTR) return true;
    }

    return false;
}


short ScaffoldingRuleMatcher::getEntryPointLocationForCell(const Cell3DPosition& pos) {
    for (int i = 0; i < 12; i++)
        if (pos == getNearestTileRootPosition(pos) + getEntryPointRelativePos(i))
            return i + RevZ_EPL;

    return -1;
}

const Cell3DPosition
ScaffoldingRuleMatcher::getEntryPointForScafComponent(const Cell3DPosition& cPos,
                                                      ScafComponent mc) {
    switch(mc) {
        case R: return getEntryPointPosition(cPos, Z_R_EPL);
        case S_Z: return getEntryPointPosition(cPos, LZ_EPL);
        case S_RevZ: return getEntryPointPosition(cPos, RZ_EPL);
        case S_LZ: return getEntryPointPosition(cPos, LZ_R_EPL);
        case S_RZ: return getEntryPointPosition(cPos, RZ_R_EPL);

        case X_1: return getEntryPointPosition(cPos, Z_R_EPL);
        case X_2: case X_3: case X_4: case X_5: return getEntryPointPosition(cPos, RZ_EPL);

        case Y_1: return getEntryPointPosition(cPos, Z_L_EPL);
        case Y_2: case Y_3: case Y_4: case Y_5: return getEntryPointPosition(cPos, LZ_EPL);

        case Z_1: case Z_2: case Z_3: case Z_4: case Z_5:
            return getEntryPointPosition(cPos, Z_EPL);

        case RevZ_1: case RevZ_2: case RevZ_3: case RevZ_4: case RevZ_5:
            return getEntryPointPosition(cPos, RevZ_EPL);

        case LZ_1: case LZ_2: case LZ_3: case LZ_4: case LZ_5:
            return getEntryPointPosition(cPos, LZ_EPL);

        case RZ_1: case RZ_2: case RZ_3: case RZ_4: case RZ_5:
            return getEntryPointPosition(cPos, RZ_EPL);

            // case EPLs
        case RevZ_EPL: return getEntryPointPosition(cPos, Z_EPL);
        case RevZ_R_EPL: return getEntryPointPosition(cPos, Z_EPL);
        case RZ_L_EPL: return getEntryPointPosition(cPos, LZ_EPL);
        case RZ_EPL: return getEntryPointPosition(cPos, LZ_EPL);
        case RZ_R_EPL: return getEntryPointPosition(cPos, LZ_EPL);
        case Z_R_EPL: return getEntryPointPosition(cPos, RevZ_EPL);
        case Z_EPL: return getEntryPointPosition(cPos, RevZ_EPL);
        case Z_L_EPL: return getEntryPointPosition(cPos, RevZ_EPL);
        case LZ_R_EPL: return getEntryPointPosition(cPos, RZ_EPL);
        case LZ_EPL: return getEntryPointPosition(cPos, RZ_EPL);
        case LZ_L_EPL: return getEntryPointPosition(cPos, RZ_EPL);
        case RevZ_L_EPL: return getEntryPointPosition(cPos, Z_EPL);
        default: throw NotImplementedException("Entry point for EPL mesh component");
    }

    return Cell3DPosition(); // unreachable
}


const Cell3DPosition
ScaffoldingRuleMatcher::getEntryPointForModuleOnIncidentBranch(const Cell3DPosition& cPos,
                                                               BranchIndex bid) {
    ScafComponent epl;
    switch(bid) {
        case RevZBranch: epl = Z_EPL; break;
        case ZBranch: epl = RevZ_EPL; break;
            // if (catomsSpawnedToVBranch[ZBranch] == 0) return getEntryPointPosition(Z_L_EPL);
            // else if (catomsSpawnedToVBranch[ZBranch] == 1)return getEntryPointPosition(Z_R_EPL);
            // else return getEntryPointPosition(Z_EPL);
        case LZBranch: epl = RZ_EPL; break;
            // if (catomsSpawnedToVBranch[LZBranch] == 0) return getEntryPointPosition(LZ_R_EPL);
            // else return getEntryPointPosition(LZ_EPL);
        case RZBranch: epl = LZ_EPL; break;
            // if (catomsSpawnedToVBranch[RZBranch] == 0) return getEntryPointPosition(RZ_R_EPL                                                                                 );
            // else return getEntryPointPosition(RZ_EPL);
        default: throw NotImplementedException("getEntryPointForModuleOnIncidentBranch: invalid bid");
    }

    return getEntryPointPosition(cPos, epl);
}

bool ScaffoldingRuleMatcher::areOnTheSameBranch(const Cell3DPosition& pos1,
                                                const Cell3DPosition& pos2) const {
    return (getTileRootPositionForMeshPosition(pos1)==getTileRootPositionForMeshPosition(pos2))
        and (not isTileRoot(pos1) or isTileRoot(pos2))
        and (getBranchIndexForNonRootPosition(pos1) == getBranchIndexForNonRootPosition(pos2));
}


/*********************************************************************/
/****************************** CSG STUFF ****************************/
/*********************************************************************/

bool ScaffoldingRuleMatcher::isInCSG(const Cell3DPosition& pos) const {
    return isInMesh(pos) and isInsideFn(pos);
}

bool ScaffoldingRuleMatcher::isWithinCSGMinus2(const Cell3DPosition& pos) const {
    if (not isInsideFn(pos)) return false;

    // This is used to work with scaffolds that are smaller than the CSG they represent by
    //  two modules.

    for (int i = 0; i < N_BRANCHES; i++) {
        BranchIndex bi = (BranchIndex)i;
        // We do not need a distance 2 for vertical positions since that would leave too much
        // space at the top of the structure and prevent the attachment of the coating to
        // the scaffold
        const Cell3DPosition &pn = pos + (i < XBranch ? 1 : 2)*getBranchUnitOffset(bi);
        if (not isInsideFn(pn)) {
            return false;
        }
    }

    return true;
}

bool ScaffoldingRuleMatcher::CSGTRAtBranchTipShouldGrowBranch(const Cell3DPosition& pos,
                                                              BranchIndex tipB,
                                                              BranchIndex growthB) const {
    if (not isInMesh(pos) or not isTileRoot(pos)) return false;

    const Cell3DPosition& tipTRPos = getTileRootAtEndOfBranch(pos, tipB);
    return shouldGrowBranch(tipTRPos, growthB);
}

Cell3DPosition ScaffoldingRuleMatcher::getSeedForCSGLayer(int z) const {
    static std::map<int, Cell3DPosition> seedForCSGLayerCache;

    if (seedForCSGLayerCache.find(z) != seedForCSGLayerCache.end())
        return seedForCSGLayerCache.at(z);

    int zReal = z * B;
    int bound = X_MAX > Y_MAX ? X_MAX : Y_MAX;
    Cell3DPosition pos;
    for (int a = 0 - zReal / 2; a < bound - zReal / 2; a++) {
        pos.set(a, a, zReal);
        if (isInCSGFn(pos - seed) and isTileRoot(pos)) {
            seedForCSGLayerCache.insert(make_pair(z, pos));
            return pos;
        }
    }

    return Cell3DPosition(-1,-1,-1);
}

BranchIndex ScaffoldingRuleMatcher::
getTileRootInsertionBranch(const Cell3DPosition& pos) const {
    int nivb = getNbIncidentVerticalBranches(pos);

// Do not use blocked EPL except if that's the only way

    if (hasIncidentBranch(pos, RevZBranch)
        and (not (hasIncidentBranch(pos, OppXBranch) and hasIncidentBranch(pos, OppYBranch))
             or nivb == 1))
        return RevZBranch; // default
    else if (hasIncidentBranch(pos, LZBranch) and
             (not (hasIncidentBranch(pos, YBranch) and hasIncidentBranch(pos, OppXBranch))
              or nivb == 1))
        return LZBranch;
    else if (hasIncidentBranch(pos, RZBranch) and
             (not (hasIncidentBranch(pos, XBranch) and hasIncidentBranch(pos, OppYBranch))
              or nivb == 1))
        return RZBranch;
    else if (hasIncidentBranch(pos, ZBranch) and
             (not (hasIncidentBranch(pos, XBranch) and hasIncidentBranch(pos, YBranch))
              or nivb == 1))
        return ZBranch;


        VS_ASSERT_MSG(false,
                       "getTileRootInsertionBranch: tile with nowhere to insert root from");

    return N_BRANCHES; // unreachable
}
