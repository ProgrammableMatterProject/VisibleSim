/**
 * @file   meshSpanningTree.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Fri Jul 13 13:27:03 2018
 *
 * @brief
 *
 *
 */

#include "meshRuleMatcher.hpp"

#include "meshAssemblyLocalRules.hpp" // for B2

#include "comm/network.h"
#include "utils/utils.h"
#include "robots/catoms3D/catoms3DWorld.h"

using namespace MeshCoating;
using namespace BaseSimulator;
using namespace utils;
using namespace Catoms3D;

std::array<Cell3DPosition, 61> MeshRuleMatcher::componentPosition = {
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
    Cell3DPosition(6, 0, 0), // X6
    Cell3DPosition(7, 0, 0), // X7

    Cell3DPosition(0, 1, 0), // Y1
    Cell3DPosition(0, 2, 0), // Y2
    Cell3DPosition(0, 3, 0), // Y3
    Cell3DPosition(0, 4, 0), // Y4
    Cell3DPosition(0, 5, 0), // Y5
    Cell3DPosition(0, 6, 0), // Y6
    Cell3DPosition(0, 7, 0), // Y7

    Cell3DPosition(0, 0, 1), // Z1
    Cell3DPosition(0, 0, 2), // Z2
    Cell3DPosition(0, 0, 3), // Z3
    Cell3DPosition(0, 0, 4), // Z4
    Cell3DPosition(0, 0, 5), // Z5
    Cell3DPosition(0, 0, 6), // Z6
    Cell3DPosition(0, 0, 7), // Z7

    Cell3DPosition(-1, -1, 1), // RevZ1
    Cell3DPosition(-2, -2, 2), // RevZ2
    Cell3DPosition(-3, -3, 3), // RevZ3
    Cell3DPosition(-4, -4, 4), // RevZ4
    Cell3DPosition(-5, -5, 5), // RevZ5
    Cell3DPosition(-6, -6, 6), // RevZ6
    Cell3DPosition(-7, -7, 7), // RevZ7

    Cell3DPosition(-1, 0, 1), // LZ1
    Cell3DPosition(-2, 0, 2), // LZ2
    Cell3DPosition(-3, 0, 3), // LZ3
    Cell3DPosition(-4, 0, 4), // LZ4
    Cell3DPosition(-5, 0, 5), // LZ5
    Cell3DPosition(-6, 0, 6), // LZ6
    Cell3DPosition(-7, 0, 7), // LZ7

    Cell3DPosition(0, -1, 1), // RZ1
    Cell3DPosition(0, -2, 2), // RZ2
    Cell3DPosition(0, -3, 3), // RZ3
    Cell3DPosition(0, -4, 4), // RZ4
    Cell3DPosition(0, -5, 5), // RZ5
    Cell3DPosition(0, -6, 6), // RZ6
    Cell3DPosition(0, -7, 7), // RZ7

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
};

string MeshRuleMatcher::component_to_string(MeshComponent comp) {
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
        case X_6: return "X_6";
        case X_7: return "X_7";
        case Y_1: return "Y_1";
        case Y_2: return "Y_2";
        case Y_3: return "Y_3";
        case Y_4: return "Y_4";
        case Y_5: return "Y_5";
        case Y_6: return "Y_6";
        case Y_7: return "Y_7";
        case Z_1: return "Z_1";
        case Z_2: return "Z_2";
        case Z_3: return "Z_3";
        case Z_4: return "Z_4";
        case Z_5: return "Z_5";
        case Z_6: return "Z_6";
        case Z_7: return "Z_7";
        case RevZ_1: return "RevZ_1";
        case RevZ_2: return "RevZ_2";
        case RevZ_3: return "RevZ_3";
        case RevZ_4: return "RevZ_4";
        case RevZ_5: return "RevZ_5";
        case RevZ_6: return "RevZ_6";
        case RevZ_7: return "RevZ_7";
        case LZ_1: return "LZ_1";
        case LZ_2: return "LZ_2";
        case LZ_3: return "LZ_3";
        case LZ_4: return "LZ_4";
        case LZ_5: return "LZ_5";
        case LZ_6: return "LZ_6";
        case LZ_7: return "LZ_7";
        case RZ_1: return "RZ_1";
        case RZ_2: return "RZ_2";
        case RZ_3: return "RZ_3";
        case RZ_4: return "RZ_4";
        case RZ_5: return "RZ_5";
        case RZ_6: return "RZ_6";
        case RZ_7: return "RZ_7";
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
    }

    return ""; // UNREACHABLE
}

Cell3DPosition MeshRuleMatcher::getPositionForComponent(MeshComponent comp) {
    return comp <= (int)RevZ_L_EPL ? componentPosition[comp] : Cell3DPosition();
}

int MeshRuleMatcher::getComponentForPosition(const Cell3DPosition& pos) {
    for (int i = 0; i <= RevZ_L_EPL; i++)
        if (componentPosition[i] == pos) return i;

    return -1;
}

MeshComponent MeshRuleMatcher::getDefaultEPLComponentForBranch(BranchIndex bi) {
    switch(bi) {
        case ZBranch: return Z_EPL;
        case RevZBranch: return RevZ_EPL;
        case RZBranch: return RZ_EPL;
        case LZBranch: return LZ_EPL;
        default: break;
    }

    cerr << "getDefaultEPLComponentForBranch: invalid input: " << bi << endl;
    VS_ASSERT(false);

    return R; // unreachable
}

MeshComponent MeshRuleMatcher::getTargetEPLComponentForBranch(BranchIndex bi) {
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

void MeshRuleMatcher::printDebugInfo(const Cell3DPosition& pos) const {
  cout << "--- DEBUG INFO: " << pos << endl;
  cout << "getTreeParentPosition(pos): " << getTreeParentPosition(pos) << endl;
  cout << "isTileRoot(pos): " << isTileRoot(pos) << endl;
  cout << "isOnPartialBorderMesh(pos): " << isOnPartialBorderMesh(pos) << endl;
  cout << "isOnXBranch(pos): " << isOnXBranch(pos) << endl;
  cout << "isOnXBorder(pos): " << isOnXBorder(pos) << endl;
  cout << "isOnYBranch(pos): " << isOnYBranch(pos) << endl;
  cout << "isOnYBorder(pos): " << isOnYBorder(pos) << endl;
  cout << "isOnZBranch(pos): " << isOnZBranch(pos) << endl;
  cout << "isOnRevZBranch(pos): " << isOnRevZBranch(pos) << endl;
  cout << "isOnRZBranch(pos): " << isOnRZBranch(pos) << endl;
  cout << "isOnLZBranch(pos): " << isOnLZBranch(pos) << endl;
  cout << "--- END DEBUG INFO ---" << endl << endl;
}

bool MeshRuleMatcher::isInGrid(const Cell3DPosition& pos) const {
    // add -2 for new tile construction using modules below R
    return isInRange(pos[0], 0 - pos[2]/ 2 - 2, X_MAX - pos[2] / 2 + 1)
        && isInRange(pos[1], 0 - pos[2] / 2 - 2, Y_MAX - pos[2] / 2 + 1)
        && isInRange(pos[2], 0, Z_MAX - 1);
}

bool MeshRuleMatcher::isInMesh(const Cell3DPosition& pos) const {
    return isInGrid(pos) and ((isOnXBranch(pos) or isOnYBranch(pos) or isOnZBranch(pos)
                               or isOnRevZBranch(pos) or isOnRZBranch(pos)
                               or isOnLZBranch(pos)
                               or isTileSupport(pos)));
}

bool MeshRuleMatcher::isInMeshOrSandbox(const Cell3DPosition& pos) const {
    Cell3DPosition meshPos = pos + Cell3DPosition(0,0,B);
    Cell3DPosition fixPos;
    fixPos.pt[0] = (meshPos[0] >= X_MAX ? meshPos[0] - B : meshPos[0]);
    fixPos.pt[1] = (meshPos[1] >= Y_MAX ? meshPos[1] - B : meshPos[1]);
    fixPos.pt[2] = (meshPos[2] >= Z_MAX ? meshPos[2] - B : meshPos[2]);

    return isInMesh(pos) or isInMesh(fixPos);
}

bool MeshRuleMatcher::isInSandbox(const Cell3DPosition& pos) const {
    return not isInMesh(pos) and isInMeshOrSandbox(pos);
}

bool MeshRuleMatcher::isOnXBranch(const Cell3DPosition& pos) const {
    return m_mod(pos[1], B) == 0 and m_mod(pos[2], B) == 0;
}

bool MeshRuleMatcher::isOnXBorder(const Cell3DPosition& pos) const {
    return pos[1] == (int)(-(int)(pos[2] / B) / 2 * B) and m_mod(pos[2], B) == 0;
}

bool MeshRuleMatcher::isOnXOppBorder(const Cell3DPosition& pos) const {
    short a = (pos[2] / B) / 2 * B;
    short b = X_MAX - 2;
    // cout << "pos: " << pos << endl;
    // cout << "a: " << a << " " << "b: " << b << endl;
    // cout << "(int)(-(int)a + b): " << (int)(-(int)a + b) << endl;
    return pos[1] == (int)(-(int)a + b) and m_mod(pos[2], B) == 0;
}

bool MeshRuleMatcher::isOnYBranch(const Cell3DPosition& pos) const {
    return m_mod(pos[0], B) == 0 and m_mod(pos[2], B) == 0;
}

bool MeshRuleMatcher::isOnYBorder(const Cell3DPosition& pos) const {
    return pos[0] == (int)(-(int)(pos[2] / B) / 2 * B) and m_mod(pos[2], B) == 0;
}

bool MeshRuleMatcher::isOnYOppBorder(const Cell3DPosition& pos) const {
    short a = (pos[2] / B) / 2 * B;
    short b = Y_MAX - 2;
    // cout << "pos: " << pos << endl;
    // cout << "a: " << a << " " << "b: " << b << endl;
    // cout << "(int)(-(int)a + b): " << (int)(-(int)a + b) << endl;
    return pos[0] == (int)(-(int)a + b) and m_mod(pos[2], B) == 0;
}

bool MeshRuleMatcher::isOnZBranch(const Cell3DPosition& pos) const {
    const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
    const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);

    return m_mod(x, B) == 0 and m_mod(y, B) == 0;
}

bool MeshRuleMatcher::isOnRevZBranch(const Cell3DPosition& pos) const {
    const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
    const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
    const int z = pos[2];

    return m_mod(x, B) == m_mod(y, B) and m_mod(z, B) == m_mod(B - x, B);
}

bool MeshRuleMatcher::isOnLZBranch(const Cell3DPosition& pos) const {
    const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
    const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
    const int z = pos[2];

    return (m_mod(x, B) + m_mod(z, B) == (int)B and m_mod(y, B) == 0);
}

bool MeshRuleMatcher::isOnRZBranch(const Cell3DPosition& pos) const {
    const int x = (pos[0] < 0 ? B + pos[0] : pos[0]);
    const int y = (pos[1] < 0 ? B + pos[1] : pos[1]);
    const int z = pos[2];

    return (m_mod(y, B) + m_mod(z, B) == (int)B and m_mod(x, B) == 0);
}

bool MeshRuleMatcher::isOnPartialBorderMesh(const Cell3DPosition& pos) const {
    return (m_mod(pos[2], B) == 0 and not isTileRoot(pos)
            and (
                (isOnXBranch(pos)
                 and (pos[0] - m_mod(pos[0], B)  < - (pos[2] / 2)) )
                or (isOnYBranch(pos)
                     and (pos[1] - m_mod(pos[1], B) < - (pos[2] / 2)) )
                ))
        or (m_mod(pos[2], B) != 0 and // Downward oblique case (ub)
            // (pos[2] - pos[2] % (int)B) is expected z of tileRoot
            ( (m_mod(pos[0], B) > 0
               and pos[0] + (int)B - m_mod(pos[0], B) >
               (int)X_MAX - (pos[2] - m_mod(pos[2], B)) / 2 )
              or (m_mod(pos[1], B) > 0
                  and pos[1] + (int)B - m_mod(pos[1], (int)B) >
                  (int)Y_MAX - (pos[2] - m_mod(pos[2], (int)B)) / 2 )
                )
            );
}

bool MeshRuleMatcher::isVerticalBranchTip(const Cell3DPosition& pos) const {
    return isNFromVerticalBranchTip(pos, 0);
}

bool MeshRuleMatcher::isNFromVerticalBranchTip(const Cell3DPosition& pos, int N) const {
    return (isOnZBranch(pos) or isOnRevZBranch(pos)
            or isOnLZBranch(pos) or isOnRZBranch(pos)) and m_mod(pos[2], B) == (B - N - 1);
}

bool MeshRuleMatcher::isEPLPivotModule(const Cell3DPosition& pos) const {
    return isNFromVerticalBranchTip(pos, 1);
}

bool MeshRuleMatcher::isBranchModule(const Cell3DPosition& pos) const {
    return (not isTileRoot(pos)) and (isOnZBranch(pos) or isOnRevZBranch(pos)
                                 or isOnLZBranch(pos) or isOnRZBranch(pos)
                                 or isOnXBranch(pos) or isOnYBranch(pos));
}

bool MeshRuleMatcher::isZBranchModule(const Cell3DPosition& pos) const {
    return (not isTileRoot(pos)) and (isOnZBranch(pos) or isOnRevZBranch(pos)
            or isOnLZBranch(pos) or isOnRZBranch(pos));
}

bool MeshRuleMatcher::isTileSupport(const Cell3DPosition& pos) const {
    return (m_mod(pos[0], B) == 1 or m_mod(pos[0], B) == B - 1)
        and (m_mod(pos[1], B) == 1 or m_mod(pos[1], B) == B - 1)
        and m_mod(pos[2], B) == 0;
}

BranchIndex MeshRuleMatcher::getBranchIndexForNonRootPosition(const Cell3DPosition& pos){
    Cell3DPosition fixPos;
    fixPos.pt[0] = (pos[0] >= X_MAX ? pos[0] - B : pos[0]);
    fixPos.pt[1] = (pos[1] >= Y_MAX ? pos[1] - B : pos[1]);
    fixPos.pt[2] = (pos[2] >= Z_MAX ? pos[2] - B : pos[2]);

    // cout << X_MAX << " " << Y_MAX << " " << Z_MAX << endl;
    // cout << fixPos << endl;
    // cout << pos << endl;
    VS_ASSERT_MSG(isInMesh(fixPos) and not isTileRoot(fixPos), "attempting to get branch index of tile root fixPosition or fixPosition outside of mesh");

    if (isOnXBranch(fixPos)) return XBranch;
    if (isOnYBranch(fixPos)) return YBranch;
    if (isOnZBranch(fixPos)) return ZBranch;
    if (isOnRevZBranch(fixPos)) return RevZBranch;
    if (isOnLZBranch(fixPos)) return LZBranch;
    if (isOnRZBranch(fixPos)) return RZBranch;

    VS_ASSERT(false); // Unreachable

    return N_BRANCHES;
}

bool MeshRuleMatcher::shouldGrowBranch(const Cell3DPosition& pos, BranchIndex bi) const {
    switch(bi) {
        case ZBranch: return shouldGrowZBranch(pos);
        case RevZBranch: return shouldGrowRevZBranch(pos);
        case RZBranch: return shouldGrowRZBranch(pos);
        case LZBranch: return shouldGrowLZBranch(pos);
        case XBranch: return shouldGrowXBranch(pos);
        case YBranch: return shouldGrowYBranch(pos);
        default: return false;
    }

    return false; //unreachable
}

bool MeshRuleMatcher::
shouldGrowZBranch(const Cell3DPosition& pos) const {
    return isTileRoot(pos) and isInMesh(Cell3DPosition(pos[0], pos[1], pos[2] + (B - 1)))
        and not isOnYOppBorder(pos) and not isOnXOppBorder(pos);
}

bool MeshRuleMatcher::
shouldGrowRevZBranch(const Cell3DPosition& pos) const {
    return isTileRoot(pos) and isInMesh(Cell3DPosition(pos[0] - (B - 1),
                                                       pos[1] - (B - 1),
                                                       pos[2] + (B - 1)));
}

bool MeshRuleMatcher::
shouldGrowLZBranch(const Cell3DPosition& pos) const {
    return isTileRoot(pos) and isInMesh(Cell3DPosition(pos[0] - (B - 1),
                                                       pos[1],
                                                       pos[2] + (B - 1)))
        and not isOnXOppBorder(pos);
}


bool MeshRuleMatcher::
shouldGrowRZBranch(const Cell3DPosition& pos) const {
    return isTileRoot(pos) and isInMesh(Cell3DPosition(pos[0],
                                                       pos[1] - (B - 1),
                                                       pos[2] + (B - 1)))
        and not isOnYOppBorder(pos);
}

bool MeshRuleMatcher::
shouldGrowXBranch(const Cell3DPosition& pos) const {
    return isTileRoot(pos) and isInMesh(Cell3DPosition(pos[0] + (B - 1),
                                                       pos[1],
                                                       pos[2]));
}

bool MeshRuleMatcher::
shouldGrowYBranch(const Cell3DPosition& pos) const {
    return isTileRoot(pos) and isInMesh(Cell3DPosition(pos[0],
                                                       pos[1] + (B - 1),
                                                       pos[2]));
}

Cell3DPosition MeshRuleMatcher::getBranchUnitOffset(int bi) const {
    switch(bi) {
        case ZBranch: return Cell3DPosition(0,0,1);
        case RevZBranch: return Cell3DPosition(-1,-1,1);
        case RZBranch: return Cell3DPosition(0,-1,1);
        case LZBranch: return Cell3DPosition(-1,0,1);
        case XBranch: return Cell3DPosition(1,0,0);
        case YBranch: return Cell3DPosition(0,1,0);
        default:
            cerr << "bi: " << bi << endl;
            VS_ASSERT_MSG(false, "invalid branch index");
    }

    return Cell3DPosition(0,0,0); // Unreachable
}

bool MeshRuleMatcher::isTileRoot(const Cell3DPosition& pos) const {
    return m_mod(pos[0], B) == 0 and m_mod(pos[1], B) == 0 and m_mod(pos[2], B) == 0;
}

bool MeshRuleMatcher::upwardBranchRulesApply(const Cell3DPosition& own,
                                                         const Cell3DPosition& other) const {
    const int zCoeff = other[2] / B;

    // Module is on branch if z is NOT a multiple of B
    return not m_mod(own[2], B) == 0
        and (
            // In that case, only allow upward transmission
            own[2] < other[2]
            // Unless neighbor is not on branch
            and (m_mod(other[2], B) != 0
                 // Transmitting to the next root only along z axis from (0,0,0)
                 or (isTileRoot(other)
                     and (
                         other[0] == (int)(-zCoeff / 2 * B)
                         and other[0] == other[1]
                         and (
                             (IS_EVEN(zCoeff)
                              // If o.z / B even, then we need to go up/backward
                              and other - own == Cell3DPosition(-1, -1, 1))
                             or
                             (IS_ODD(zCoeff)
                              // if o.z / B odd, then we need to go up/forward
                              and other - own == Cell3DPosition(0, 0, 1))
                             )
                         )
                     )
                )
            );
}

bool MeshRuleMatcher::planarBranchRulesApply(const Cell3DPosition& own,
                                                         const Cell3DPosition& other) const {
    // Module is on plan if z is a multiple of B
    return m_mod(own[2], B) == 0 and own[2] == other[2]
        and (
            // In that case, only allow transmission to increasing x...
            // (except if other is a root as transmission to roots occur along y axis,
            //   and we are not on lower border)
            (own[0] < other[0] and
             (not isTileRoot(other) or (own[1] + own[2] / 2) < (int)B))
            // ... or increasing y.
            or own[1] < other[1]
            );
}

bool MeshRuleMatcher::meshRootBranchRulesApply(const Cell3DPosition& own,
                                                           const Cell3DPosition& other) const {
    // Mesh root is responsible for upward propagation
    return isTileRoot(own)
        and (
            // In that case, only allow transmission to increasing z, in all directions
             own[2] < other[2]
             );
}

bool MeshRuleMatcher::partialBorderMeshRulesApply(const Cell3DPosition& own,
                                                              const Cell3DPosition& other) const {
    // Gotta decide that we are on a mesh with no root using xmax, ymax, xmax;
    return isOnPartialBorderMesh(other)
        and (
            // Mesh root initiates on a border initiates normally forbidden transmission
            isTileRoot(own)
            or
            (isOnPartialBorderMesh(own)
             and ((own[2] % B == 0 and own[2] == other[2] and (own[0] > other[0]
                                                               or own[1] > other[1]))
                  or (own[2] % B != 0 and other[2] < own[2]))
                ));
}

bool MeshRuleMatcher::shouldSendToNeighbor(const Cell3DPosition& own,
                                                       const Cell3DPosition& other) const {
    return (not isOnPartialBorderMesh(own)
            and (
                planarBranchRulesApply(own, other)
                or meshRootBranchRulesApply(own, other)
                or upwardBranchRulesApply(own, other)
                )
        )
        or partialBorderMeshRulesApply(own, other);
}

const Cell3DPosition
MeshRuleMatcher::getTreeParentPosition(const Cell3DPosition& pos) const {
    if (isTileRoot(pos)) {
        const int zCoeff = pos[2] / B;
        // Mesh Root
        if (pos[0] == 0 and pos[1] == 0 and pos[2] == 0) return pos;
        // Upward propagating branch, parent is branch predecessor
        else if (pos[0] == (int)(-zCoeff / 2 * B) and pos[1] == pos[0]) {
            return IS_EVEN(zCoeff) ?
                pos + Cell3DPosition(1, 1, -1) : pos + Cell3DPosition(0, 0, -1);
        }
    }
\
    if (isOnPartialBorderMesh(pos)) {
        // parent is either x - 1 or y - 1 if planar
        if (isOnYBranch(pos)) return pos + Cell3DPosition(0,1,0);
        else if (isOnXBranch(pos)) return pos + Cell3DPosition(1,0,0);
        //  or z + 1 if on an incomplete downward branch
        else if (isOnZBranch(pos)) return pos + Cell3DPosition(0,0,1);
        else if (isOnRevZBranch(pos)) return pos + Cell3DPosition(-1,-1,1);
        else if (isOnLZBranch(pos)) return pos + Cell3DPosition(-1,0,1);
        else if (isOnRZBranch(pos)) return pos + Cell3DPosition(0,-1,1);

        assert(false);
    }

    // Planar cases X and Y
    else if (isOnYBranch(pos) and !isOnXBorder(pos)) return pos + Cell3DPosition(0,-1,0);
    else if (isOnXBranch(pos) and !isOnYBorder(pos)) return pos + Cell3DPosition(-1,0,0);
    // Branch cases
    else if (isOnZBranch(pos)) return pos + Cell3DPosition(0,0,-1);
    else if (isOnRevZBranch(pos)) return pos + Cell3DPosition(1,1,-1);
    else if (isOnLZBranch(pos)) return pos + Cell3DPosition(1,0,-1);
    else if (isOnRZBranch(pos)) return pos + Cell3DPosition(0,1,-1);

    assert(false);
}

unsigned int MeshRuleMatcher::
getNumberOfExpectedSubTreeConfirms(const Cell3DPosition& pos) const {
    unsigned int expectedConfirms = 0;
    for (const Cell3DPosition& nPos :
             Catoms3D::getWorld()->lattice->getActiveNeighborCells(pos)) {
        if (shouldSendToNeighbor(pos, nPos)) expectedConfirms++;
    }

    OUTPUT << "NumberExpectedConfirms for " << pos << " : " << expectedConfirms << endl;

    return expectedConfirms;
}

short MeshRuleMatcher::determineBranchForPosition(const Cell3DPosition& pos) const {
    // cout << "norm: " <<pos << endl;

    if (isInMesh(pos) and not isTileRoot(pos)) {
        if (isOnZBranch(pos)) return ZBranch;
        if (isOnRevZBranch(pos)) return RevZBranch;
        if (isOnLZBranch(pos)) return LZBranch;
        if (isOnRZBranch(pos)) return RZBranch;
        if (isOnXBranch(pos)) return XBranch;
        if (isOnYBranch(pos)) return YBranch;
    }

    return -1;
}

const Cell3DPosition
MeshRuleMatcher::getNearestTileRootPosition(const Cell3DPosition& pos) const {
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
MeshRuleMatcher::getTileRootPositionForMeshPosition(const Cell3DPosition& pos) const {
    return Cell3DPosition(pos[0] - m_mod(pos[0], B),
                          pos[1] - m_mod(pos[1], B),
                          pos[2] - m_mod(pos[2], B));
}

const Cell3DPosition
MeshRuleMatcher::getSupportPositionForPosition(const Cell3DPosition& pos) const {
    if (isOnZBranch(pos)) return Cell3DPosition(-1, -1, 0);
    if (isOnRevZBranch(pos)) return Cell3DPosition(1, 1, 0);
    if (isOnRZBranch(pos)) return Cell3DPosition(-1, 1, 0);
    if (isOnLZBranch(pos)) return Cell3DPosition(1, -1, 0);

    return Cell3DPosition();
}

AgentRole MeshRuleMatcher::getRoleForPosition(const Cell3DPosition& pos) const {
    if (not isInMesh(pos)) return AgentRole::FreeAgent;
    else {
        if (isTileRoot(pos)) return AgentRole::Coordinator;
        else if (isVerticalBranchTip(pos)) return AgentRole::ActiveBeamTip;
        else if (isTileSupport(pos)) return AgentRole::Support;
        else return AgentRole::PassiveBeam;
    }
}

const Cell3DPosition MeshRuleMatcher::getPositionForMeshComponent(MeshComponent mc) {
    switch(mc) {
        case R: return Cell3DPosition(0,0,0);
        case S_Z: return Cell3DPosition(1,1,0);
        case S_RevZ: return Cell3DPosition(-1,-1,0);
        case S_LZ: return Cell3DPosition(-1,1,0);
        case S_RZ: return Cell3DPosition(1,-1,0);

        case X_1: case X_2: case X_3: case X_4: case X_5: case X_6: case X_7:
            return Cell3DPosition(1 * (mc - X_1 + 1), 0, 0);

        case Y_1: case Y_2: case Y_3: case Y_4: case Y_5: case Y_6: case Y_7:
            return Cell3DPosition(0, 1 * (mc - Y_1 + 1), 0);

        case Z_1: case Z_2: case Z_3: case Z_4: case Z_5: case Z_6: case Z_7:
            return Cell3DPosition(0, 0, 1 * (mc - Z_1 + 1));

        case RevZ_1: case RevZ_2: case RevZ_3: case RevZ_4: case RevZ_5: case RevZ_6: case RevZ_7:
            return Cell3DPosition(-1 * (mc - RevZ_1 + 1), -1 * (mc - RevZ_1 + 1),
                                  1 * (mc - RevZ_1 + 1));

        case LZ_1: case LZ_2: case LZ_3: case LZ_4: case LZ_5: case LZ_6: case LZ_7:
            return Cell3DPosition(-1 * (mc - LZ_1 + 1), 0, 1 * (mc - LZ_1 + 1));

        case RZ_1: case RZ_2: case RZ_3: case RZ_4: case RZ_5: case RZ_6: case RZ_7:
            return Cell3DPosition(0, -1 * (mc - RZ_1 + 1), 1 * (mc - RZ_1 + 1));

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
    }

    return Cell3DPosition(); // unreachable
}

int MeshRuleMatcher::getBranchIndexForMeshComponent(MeshComponent mc) {
    switch(mc) {
        case X_1: case X_2: case X_3: case X_4: case X_5: case X_6: case X_7:
            return XBranch;

        case Y_1: case Y_2: case Y_3: case Y_4: case Y_5: case Y_6: case Y_7:
            return YBranch;

        case Z_1: case Z_2: case Z_3: case Z_4: case Z_5: case Z_6: case Z_7:
            return ZBranch;

        case RevZ_1: case RevZ_2: case RevZ_3: case RevZ_4: case RevZ_5: case RevZ_6: case RevZ_7:
            return RevZBranch;

        case LZ_1: case LZ_2: case LZ_3: case LZ_4: case LZ_5: case LZ_6: case LZ_7:
            return LZBranch;

        case RZ_1: case RZ_2: case RZ_3: case RZ_4: case RZ_5: case RZ_6: case RZ_7:
            return RZBranch;
        default: break;
    }

    return -1;
}

const Cell3DPosition
MeshRuleMatcher::getPositionForChildTileMeshComponent(MeshComponent mc) const {
    switch(mc) {
        // case EPLs
        // case RevZ_EPL: return Cell3DPosition(-1,-1,-1);
        // case RevZ_R_EPL: return Cell3DPosition(0,-1,-1);
        // case RZ_L_EPL: return Cell3DPosition(1,-1,-1);
        // case RZ_EPL: return Cell3DPosition(2,-1,-1);
        case RZ_R_EPL:
            return getPositionForMeshComponent(mc) + Cell3DPosition(-(short)B,0,B);
        case Z_R_EPL:
            return getPositionForMeshComponent(mc) + Cell3DPosition(-(short)B,-(short)B,B);
        // case Z_EPL: return Cell3DPosition(2,2,-1);
        // case Z_L_EPL: return Cell3DPosition(1,2,-1);
        case LZ_R_EPL:
            return getPositionForMeshComponent(mc) + Cell3DPosition(0,-(short)B,B);
        // case LZ_EPL: return Cell3DPosition(-1,2,-1);
        // case LZ_L_EPL: return Cell3DPosition(-1,1,-1);
        // case RevZ_L_EPL: return Cell3DPosition(-1,0,-1);
        default: VS_ASSERT_MSG(false, "Child tile mesh component must be an EPL");
    }

    return Cell3DPosition(); // unreachable
}

const Color& MeshRuleMatcher::getColorForPosition(const Cell3DPosition& pos) const {
    switch(getRoleForPosition(pos)) {
        case Coordinator: return GREEN;
        case Support: return YELLOW;
        case FreeAgent: return ORANGE;
        case ActiveBeamTip: return RED;
        case PassiveBeam: return BLUE;
    }

    return GREY;
}

const vector<Cell3DPosition> MeshRuleMatcher::getAllGroundTileRootPositionsForMesh() const {
    vector<Cell3DPosition> tileRoots;

    for (int x = 0; x < X_MAX; x+=B)
        for (int y = 0; y < Y_MAX; y+=B)
            tileRoots.push_back(Cell3DPosition(x,y,0));

    return tileRoots;
}

Cell3DPosition MeshRuleMatcher::getPositionOfBranchTipUnder(BranchIndex bi) {
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

BranchIndex MeshRuleMatcher::getAlternateBranchIndex(BranchIndex bi) const {
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

bool MeshRuleMatcher::isOnXPyramidBorder(const Cell3DPosition& pos) const {
    return pos[1] == 0 and m_mod(pos[2], B) == 0;
}

bool MeshRuleMatcher::isOnXOppPyramidBorder(const Cell3DPosition& pos) const {
    return pos[1] == Y_MAX - pos[2] - 2 and m_mod(pos[2], B) == 0;
}

bool MeshRuleMatcher::isOnYPyramidBorder(const Cell3DPosition& pos) const {
    return pos[0] == 0 and m_mod(pos[2], B) == 0;
}

bool MeshRuleMatcher::isOnYOppPyramidBorder(const Cell3DPosition& pos) const {
    return pos[0] == X_MAX - pos[2] - 2 and m_mod(pos[2], B) == 0;
}

bool MeshRuleMatcher::isInPyramid(const Cell3DPosition& pos) const {
    return  isInMesh(pos) and isInRange(pos[0], 0, X_MAX - pos[2] - 2)
        and isInRange(pos[1], 0, Y_MAX - pos[2] - 2);
}

const Cell3DPosition
MeshRuleMatcher::getTileRootAtEndOfBranch(const Cell3DPosition& trRef,
                                          BranchIndex bi) const {
    if (not isInMesh(trRef)) return trRef;
    // cout << "trRef: " << trRef << " - bi: " << bi << " - res: "
    //      << trRef + B * getBranchUnitOffset(bi) << endl;
    return trRef + B * getBranchUnitOffset(bi);
}

bool MeshRuleMatcher::shouldGrowPyramidBranch(const Cell3DPosition& pos,
                                              BranchIndex bi) const {
    return shouldGrowBranch(pos, bi)
        and isInPyramid(getTileRootAtEndOfBranch(pos, bi));
}

bool MeshRuleMatcher::pyramidTRAtBranchTipShouldGrowBranch(const Cell3DPosition& pos,
                                                           BranchIndex tipB,
                                                           BranchIndex growthB) const {
    if (not isInMesh(pos) or not isTileRoot(pos)) return false;

    const Cell3DPosition& tipTRPos = getTileRootAtEndOfBranch(pos, tipB);
    return shouldGrowPyramidBranch(tipTRPos, growthB);
}

string MeshRuleMatcher::roleToString(AgentRole ar) {
    switch(ar) {
        case FreeAgent: return "FreeAgent";
        case Coordinator: return "Coordinator";
        case PassiveBeam: return "Beam";
        case ActiveBeamTip: return "Relay";
        case Support: return "Relay";
    }

    return "";
}

string MeshRuleMatcher::branch_to_string(BranchIndex bi) {
    switch(bi) {
        case RevZBranch: return "RevZBranch";
        case ZBranch: return "ZBranch";
        case RZBranch: return "RZBranch";
        case LZBranch: return "LZBranch";
        case XBranch: return "XBranch";
        case YBranch: return "YBranch";
        default: return "";
    }

    return "";
}

BranchIndex MeshRuleMatcher::getBranchForEPL(MeshComponent epl) {
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
        default: VS_ASSERT_MSG(false, "getBranchForEPL: input epl is not an EPL");
    }

    return N_BRANCHES;
}

const Cell3DPosition MeshRuleMatcher::getTargetEPLPositionForBranch(BranchIndex bi) {
    switch (bi) {
        case RevZBranch: return Cell3DPosition(-(B2 - 2), -(B2 - 2), B2 - 1);
        case RZBranch: return Cell3DPosition(-1, -(B2 - 2), B2 - 1);
        case ZBranch: return Cell3DPosition(-1, -1, B2 - 1);
        case LZBranch: return Cell3DPosition(-(B2 - 2), -1, B2 - 1);
        default:
            cerr << "getTargetEPLPositionForBranch(" << bi << ")" << endl;
            VS_ASSERT_MSG(false, "getTargetEPLPositionForBranch: input is not a valid branch");
    }

    return Cell3DPosition(); // unreachable
}

int MeshRuleMatcher::getPyramidDimension() const {
    return (Z_MAX / B) + 1;
}
