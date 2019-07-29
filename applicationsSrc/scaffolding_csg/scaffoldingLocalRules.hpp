/**
 * @file   meshAssemblyLocalRules.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Thu Oct 18 22:49:43 2018
 *
 * @brief
 *
 *
 */


#ifndef MESHASSEMBLYLOCALRULES_HPP__
#define MESHASSEMBLYLOCALRULES_HPP__

#include "cell3DPosition.h"

#include <bitset>
#include <utility>

using namespace BaseSimulator;
using namespace std;

#define B2 6 // This one is used to initialize BlockCode-wide constants

enum LR_EPL {
    LR_RevZ_EPL = 35,
    LR_RevZ_R_EPL = 36,
    LR_RZ_L_EPL = 37,
    LR_RZ_EPL = 38,
    LR_RZ_R_EPL = 39,
    LR_Z_R_EPL = 40,
    LR_Z_EPL = 41,
    LR_Z_L_EPL = 42,
    LR_LZ_R_EPL = 43,
    LR_LZ_EPL = 44,
    LR_LZ_L_EPL = 45,
    LR_RevZ_L_EPL = 46,

    LR_RevZ_EPL_ALT = 135, // FIXME: TODO: This is due to a conflict with S_Z on iBorders
    LR_RZ_EPL_ALT = 138, // FIXME: TODO: This is due to a conflict with S_Z on iBorders
    LR_Z_EPL_ALT = 141, // FIXME: TODO: This is due to a conflict with S_Z on iBorders
    LR_LZ_EPL_ALT = 144, // FIXME: TODO: This is due to a conflict with S_LZ on Y OPP act

    LR_RevZ_EPL_RIGHT = 235, // Used for reaching S_RevZ through the righ side of the ZBranch
};

class LRKeyTuple {
    const std::bitset<12> lnBitset;
    const short EPL; // Corresponds to an EPL as defined in MeshRuleMatcher.h
    const Cell3DPosition tPos;
    const short step;
public:
    constexpr LRKeyTuple(int ln, const short _EPL,
                         const Cell3DPosition& _tPos, const short _step)
        : lnBitset(bitset<12>(ln)), EPL(_EPL), tPos(_tPos), step(_step) {}

    constexpr LRKeyTuple(const bitset<12>& _lnBitset, const short _EPL,
                         const Cell3DPosition& _tPos, const short _step)
        : lnBitset(bitset<12>(_lnBitset)), EPL(_EPL), tPos(_tPos), step(_step) {}

    bool operator<(const LRKeyTuple &lrkt) const {
        if (lnBitset.to_ulong() < lrkt.lnBitset.to_ulong()) return true;
        else if (lnBitset.to_ulong() > lrkt.lnBitset.to_ulong()) return false;
        else if (step < lrkt.step) return true;
        else if (step > lrkt.step) return false;
        else if (EPL < lrkt.EPL) return true;
        else if (EPL > lrkt.EPL) return false;
        else return tPos < lrkt.tPos;
    }

    bool operator==(const LRKeyTuple &lrkt) const {
        return lnBitset.to_ulong() == lrkt.lnBitset.to_ulong()
            and EPL == lrkt.EPL
            and tPos == lrkt.tPos
            and step == lrkt.step;
    }
};

// CONTINUOUS FEEDING RULES
static const std::map <const LRKeyTuple, const Cell3DPosition> localMotionRules =
{
    // Corner XY Cases
    { LRKeyTuple(0x400, LR_RZ_EPL, Cell3DPosition(-1, -1, 0), 3),
      Cell3DPosition(0, 1, -1) }, // S_RevZ 3
    { LRKeyTuple(0xC03, LR_RZ_EPL, Cell3DPosition(-1, -1, 0), 4),
      Cell3DPosition(-1, 0, 0) }, // S_RevZ 4

    // Border OppX Cases
    { LRKeyTuple(0x380, LR_Z_EPL, Cell3DPosition(0, 0, 2), 3),
      Cell3DPosition(0, -1, 1) }, // Z2 3

    // Border OppY Cases
    { LRKeyTuple(0x940, LR_RZ_EPL, Cell3DPosition(0, -2, 2), 3),
      Cell3DPosition(-1, -1, 1) }, // RZ2 3

    // Pyramid Corner cases
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(1, 1, 0), 3),
      Cell3DPosition(1, 0, -1) }, // S_Z 3
    { LRKeyTuple(0x3C0, LR_LZ_EPL, Cell3DPosition(1, 1, 0), 4),
      Cell3DPosition(1, 0, 0) }, // S_Z 4

    // Normal Cases
    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(0, 0, 0), 1),
      Cell3DPosition(0, -1, 0) }, // R 1
    { LRKeyTuple(0x840, LR_Z_EPL, Cell3DPosition(0, 0, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // R 2
    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(0, 0, 0), 3),
      Cell3DPosition(-1, -1, 0) }, // R 3

    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(1, -1, 0), 1),
      Cell3DPosition(0,1, 0) }, // S_RZ 1
    { LRKeyTuple(0x140, LR_RZ_EPL, Cell3DPosition(1, -1, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // S_RZ 2
    { LRKeyTuple(0x142, LR_RZ_EPL, Cell3DPosition(1, -1, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // S_RZ 2 BIS

    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(-1, 1, 0), 1),
      Cell3DPosition(1, 0, 0) }, // S_LZ 1
    { LRKeyTuple(0x180, LR_LZ_EPL, Cell3DPosition(-1, 1, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // S_LZ 2
    { LRKeyTuple(0x181, LR_LZ_EPL, Cell3DPosition(-1, 1, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // S_LZ 2 BIS

    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(0, 1, 0), 1),
      Cell3DPosition(-1, 0, 0) }, // Y1 1
    { LRKeyTuple(0x280, LR_Z_EPL, Cell3DPosition(0, 1, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // Y1 2
    { LRKeyTuple(0x2C0, LR_Z_EPL, Cell3DPosition(0, 1, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // Y1 2 BIS

    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(1, 0, 0), 1),
      Cell3DPosition(0, -1, 0) }, // X1 1
    { LRKeyTuple(0x840, LR_Z_EPL, Cell3DPosition(1, 0, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // X1 2
    { LRKeyTuple(0x8C0, LR_Z_EPL, Cell3DPosition(1, 0, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // X1 2 BIS

    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(1, 1, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // S_Z 1
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(-1, -1, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // S_RevZ 1
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(1, 1, 0), 2),
      Cell3DPosition(1, 0, 0) }, // S_Z 2
    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(-1, -1, 0), 2),
      Cell3DPosition(-1, 0, 0) }, // S_RevZ 2
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(1, 1, 0), 3),
      Cell3DPosition(1, 0, 0) }, // S_Z 3
    { LRKeyTuple(0xD00, LR_RZ_EPL, Cell3DPosition(-1, -1, 0), 3),
      Cell3DPosition(-1, 0, 0) }, // S_RevZ 3
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(2, 0, 0), 1),
      Cell3DPosition(0, 0, 1) }, // X2 1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(0, 2, 0), 1),
      Cell3DPosition(0, 0, 1) }, // Y2 1
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(1, 1, 0), 4),
      Cell3DPosition(1, 0, -1) }, // S_Z 4
    { LRKeyTuple(0x600, LR_RZ_EPL, Cell3DPosition(-1, -1, 0), 4),
      Cell3DPosition(0, 1, -1) }, // S_RevZ 4
    { LRKeyTuple(0x040, LR_RZ_EPL, Cell3DPosition(2, 0, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // X2 2
    { LRKeyTuple(0x080, LR_LZ_EPL, Cell3DPosition(0, 2, 0), 2),
      Cell3DPosition(0, -1, 1) }, // Y2 2
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(2, 0, 0), 3),
      Cell3DPosition(1, 1, -1) }, // X2 3
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(0, 2, 0), 3),
      Cell3DPosition(1, 1, -1) }, // Y2 3
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 0, 1), 1),
      Cell3DPosition(-1, -1, 2) }, // Z1 1
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-1, -1, 1), 1),
      Cell3DPosition(-1, -1, 2) }, // RevZ1 1
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(3, 0, 0), 1),
      Cell3DPosition(0, 0, 1) }, // X3 1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(0, 3, 0), 1),
      Cell3DPosition(0, 0, 1) }, // Y3 1
    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(0, 0, 1), 2),
      Cell3DPosition(-1, -1, 0) }, // Z1 2
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(-1, -1, 1), 2),
      Cell3DPosition(1, 1, 0) }, // RevZ1 2
    { LRKeyTuple(0x042, LR_RZ_EPL, Cell3DPosition(3, 0, 0), 2),
      Cell3DPosition(1, 1, 0) }, // X3 2
    { LRKeyTuple(0x081, LR_LZ_EPL, Cell3DPosition(0, 3, 0), 2),
      Cell3DPosition(1, 1, 0) }, // Y3 2
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 0, 2), 1),
      Cell3DPosition(-1, -1, 2) }, // Z2 1
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-2, -2, 2), 1),
      Cell3DPosition(-1, -1, 2) }, // RevZ2 1
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(4, 0, 0), 1),
      Cell3DPosition(0, 0, 1) }, // X4 1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(0, 4, 0), 1),
      Cell3DPosition(0, 0, 1) }, // Y4 1
    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(0, 0, 2), 2),
      Cell3DPosition(-1, 0, 0) }, // Z2 2
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(-2, -2, 2), 2),
      Cell3DPosition(1, 0, 0) }, // RevZ2 2
    { LRKeyTuple(0x042, LR_RZ_EPL, Cell3DPosition(4, 0, 0), 2),
      Cell3DPosition(0, 0, 1) }, // X4 2
    { LRKeyTuple(0x081, LR_LZ_EPL, Cell3DPosition(0, 4, 0), 2),
      Cell3DPosition(0, 0, 1) }, // Y4 2
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 0, 3), 1),
      Cell3DPosition(-1, -1, 2) }, // Z3 1
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-3, -3, 3), 1),
      Cell3DPosition(-1, -1, 2) }, // RevZ3 1
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(5, 0, 0), 1),
      Cell3DPosition(0, 0, 1) }, // X5 1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(0, 5, 0), 1),
      Cell3DPosition(0, 0, 1) }, // Y5 1
    { LRKeyTuple(0xB80, LR_Z_EPL, Cell3DPosition(0, 0, 2), 3),
      Cell3DPosition(0, -1, 1) }, // Z2 3
    { LRKeyTuple(0x280, LR_Z_EPL, Cell3DPosition(0, 0, 2), 3),
      Cell3DPosition(0, -1, 1) }, // Z2 3 CUBE XOPP BORDER
    { LRKeyTuple(0xE02, LR_RevZ_EPL, Cell3DPosition(-2, -2, 2), 3),
      Cell3DPosition(-1, 0, 1) }, // RevZ2 3
    { LRKeyTuple(0x802, LR_RevZ_EPL, Cell3DPosition(-2, -2, 2), 3),
      Cell3DPosition(-1, 0, 1) }, // RevZ2 3 EDGE ORIGIN
    { LRKeyTuple(0xc02, LR_RevZ_EPL, Cell3DPosition(-2, -2, 2), 3),
      Cell3DPosition(-1, 0, 1) }, // RevZ2 2 bis
    { LRKeyTuple(0xC00, LR_RZ_EPL, Cell3DPosition(4, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X4 3
    { LRKeyTuple(0x600, LR_LZ_EPL, Cell3DPosition(0, 4, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y4 3
    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(4, 0, 0), 4),
      Cell3DPosition(1, 1, -1) }, // X4 4
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(0, 4, 0), 4),
      Cell3DPosition(1, 1, -1) }, // Y4 4
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 0, 3), 2),
      Cell3DPosition(-1, -1, 2) }, // Z3 2
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-3, -3, 3), 2),
      Cell3DPosition(-1, -1, 2) }, // RevZ3 2
    { LRKeyTuple(0x042, LR_RZ_EPL, Cell3DPosition(5, 0, 0), 2),
      Cell3DPosition(0, 0, 1) }, // X5 2
    { LRKeyTuple(0x081, LR_LZ_EPL, Cell3DPosition(0, 5, 0), 2),
      Cell3DPosition(0, 0, 1) }, // Y5 2
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 0, 4), 1),
      Cell3DPosition(-1, -1, 2) }, // Z4 1
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-4, -4, 4), 1),
      Cell3DPosition(-1, -1, 2) }, // RevZ4 1
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -1, 1), 1),
      Cell3DPosition(-1, -1, 2) }, // RZ1 1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-1, 0, 1), 1),
      Cell3DPosition(-1, -1, 2) }, // LZ1 1
    { LRKeyTuple(0xC00, LR_RZ_EPL, Cell3DPosition(5, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X5 3
    { LRKeyTuple(0x600, LR_LZ_EPL, Cell3DPosition(0, 5, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y5 3
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 0, 4), 2),
      Cell3DPosition(-1, 0, 1) }, // Z4 2
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-4, -4, 4), 2),
      Cell3DPosition(0, -1, 1) }, // RevZ4 2
    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(0, -1, 1), 2),
      Cell3DPosition(-1, 1, 0) }, // RZ1 2
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(-1, 0, 1), 2),
      Cell3DPosition(1, -1, 0) }, // LZ1 2
    { LRKeyTuple(0xC00, LR_RZ_EPL, Cell3DPosition(5, 0, 0), 4),
      Cell3DPosition(1, 1, 0) }, // X5 4
    { LRKeyTuple(0x600, LR_LZ_EPL, Cell3DPosition(0, 5, 0), 4),
      Cell3DPosition(1, 1, 0) }, // Y5 4
    { LRKeyTuple(0xA0, LR_Z_EPL, Cell3DPosition(0, 0, 4), 3),
      Cell3DPosition(-1, -1, 2) }, // Z4 3
    { LRKeyTuple(0xA, LR_RevZ_EPL, Cell3DPosition(-4, -4, 4), 3),
      Cell3DPosition(-1, -1, 2) }, // RevZ4 3
    { LRKeyTuple(0x100, LR_RZ_EPL, Cell3DPosition(5, 0, 0), 5),
      Cell3DPosition(1, 0, -1) }, // X5 5
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(0, 5, 0), 5),
      Cell3DPosition(0, 1, -1) }, // Y5 5
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 0, 5), 1),
      Cell3DPosition(-1, -1, 2) }, // Z5 1
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-5, -5, 5), 1),
      Cell3DPosition(-1, -1, 2) }, // RevZ5 1
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -2, 2), 1),
      Cell3DPosition(-1, -1, 2) }, // RZ2 1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-2, 0, 2), 1),
      Cell3DPosition(-1, -1, 2) }, // LZ2 1
    { LRKeyTuple(0x200, LR_Z_EPL, Cell3DPosition(0, 0, 4), 4),
      Cell3DPosition(1, 0, 0) }, // Z4 4
    { LRKeyTuple(0x800, LR_RevZ_EPL, Cell3DPosition(-4, -4, 4), 4),
      Cell3DPosition(-1, 0, 0) }, // RevZ4 4
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 0, 5), 2),
      Cell3DPosition(-1, 0, 1) }, // Z5 2
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-5, -5, 5), 2),
      Cell3DPosition(0, -1, 1) }, // RevZ5 2
    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(0, -2, 2), 2),
      Cell3DPosition(0, 1, 0) }, // RZ2 2
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(-2, 0, 2), 2),
      Cell3DPosition(0, -1, 0) }, // LZ2 2
    { LRKeyTuple(0xA0, LR_Z_EPL, Cell3DPosition(0, 0, 5), 3),
      Cell3DPosition(0, 0, 1) }, // Z5 3
    { LRKeyTuple(0xA, LR_RevZ_EPL, Cell3DPosition(-5, -5, 5), 3),
      Cell3DPosition(-1, -1, 2) }, // RevZ5 3
    { LRKeyTuple(0xD40, LR_RZ_EPL, Cell3DPosition(0, -2, 2), 3),
      Cell3DPosition(-1, -1, 1) }, // RZ2 3
    { LRKeyTuple(0x140, LR_RZ_EPL, Cell3DPosition(0, -2, 2), 3),
      Cell3DPosition(-1, -1, 1) }, // RZ2 3 CUBE YOPP BORDER
    { LRKeyTuple(0x701, LR_LZ_EPL, Cell3DPosition(-2, 0, 2), 3),
      Cell3DPosition(0, 0, 1) }, // LZ2 3
    { LRKeyTuple(0x401, LR_LZ_EPL, Cell3DPosition(-2, 0, 2), 3),
      Cell3DPosition(0, 0, 1) }, // LZ2 3 CUBE Y BORDER
    { LRKeyTuple(0x601, LR_LZ_EPL, Cell3DPosition(-2, 0, 2), 3),
      Cell3DPosition(0, 0, 1) }, // LZ2 3 TER
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -3, 3), 1),
      Cell3DPosition(-1, -1, 2) }, // RZ3 1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-3, 0, 3), 1),
      Cell3DPosition(-1, -1, 2) }, // LZ3 1
    { LRKeyTuple(0xA0, LR_Z_EPL, Cell3DPosition(0, 0, 5), 4),
      Cell3DPosition(-1, -1, 2) }, // Z5 4
    { LRKeyTuple(0x840, LR_RevZ_EPL, Cell3DPosition(-5, -5, 5), 4),
      Cell3DPosition(-1, -1, 1) }, // RevZ5 4
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -3, 3), 2),
      Cell3DPosition(-1, -1, 2) }, // RZ3 2
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-3, 0, 3), 2),
      Cell3DPosition(-1, -1, 2) }, // LZ3 2
    { LRKeyTuple(0x200, LR_Z_EPL, Cell3DPosition(0, 0, 5), 5),
      Cell3DPosition(1, 0, 0) }, // Z5 5
    { LRKeyTuple(0x800, LR_RevZ_EPL, Cell3DPosition(-5, -5, 5), 5),
      Cell3DPosition(-1, 0, 0) }, // RevZ5 5
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -4, 4), 1),
      Cell3DPosition(-1, -1, 2) }, // RZ4 1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-4, 0, 4), 1),
      Cell3DPosition(-1, -1, 2) }, // LZ4 1
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -4, 4), 2),
      Cell3DPosition(0, 0, 1) }, // RZ4 2
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-4, 0, 4), 2),
      Cell3DPosition(-1, -1, 1) }, // LZ4 2
    { LRKeyTuple(0x050, LR_RZ_EPL, Cell3DPosition(0, -4, 4), 3),
      Cell3DPosition(0, -1, 1) }, // RZ4 3
    { LRKeyTuple(0x005, LR_LZ_EPL, Cell3DPosition(-4, 0, 4), 3),
      Cell3DPosition(-1, 0, 1) }, // LZ4 3
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -5, 5), 1),
      Cell3DPosition(-1, -1, 2) }, // RZ5 1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-5, 0, 5), 1),
      Cell3DPosition(-1, -1, 2) }, // LZ5 1
    { LRKeyTuple(0x040, LR_RZ_EPL, Cell3DPosition(0, -4, 4), 4),
      Cell3DPosition(-1, -1, 1) }, // RZ4 4
    { LRKeyTuple(0x001, LR_LZ_EPL, Cell3DPosition(-4, 0, 4), 4),
      Cell3DPosition(0, 0, 1) }, // LZ4 4
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -5, 5), 2),
      Cell3DPosition(0, 0, 1) }, // RZ5 2
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-5, 0, 5), 2),
      Cell3DPosition(-1, -1, 1) }, // LZ5 2
    { LRKeyTuple(0x050, LR_RZ_EPL, Cell3DPosition(0, -5, 5), 3),
      Cell3DPosition(0, -1, 1) }, // RZ5 3
    { LRKeyTuple(0x005, LR_LZ_EPL, Cell3DPosition(-5, 0, 5), 3),
      Cell3DPosition(-1, 0, 1) }, // LZ5 3
    { LRKeyTuple(0x050, LR_RZ_EPL, Cell3DPosition(0, -5, 5), 4),
      Cell3DPosition(-1, -1, 2) }, // RZ5 4
    { LRKeyTuple(0x005, LR_LZ_EPL, Cell3DPosition(-5, 0, 5), 4),
      Cell3DPosition(-1, -1, 2) }, // LZ5 4
    { LRKeyTuple(0x100, LR_RZ_EPL, Cell3DPosition(0, -5, 5), 5),
      Cell3DPosition(0, -1, 0) }, // RZ5 5
    { LRKeyTuple(0x400, LR_LZ_EPL, Cell3DPosition(-5, 0, 5), 5),
      Cell3DPosition(0, 1, 0) }, // LZ5 5

    // Vertical Branches Climbing Rules
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(-1, -(B2 - 2), (B2 - 1)), 1),
            Cell3DPosition(-1, -1, 2) }, // LZ_EPL1
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(-1, -(B2 - 2), (B2 - 1)), 2),
            Cell3DPosition(0, 0, 1) }, // LZ_EPL2
    { LRKeyTuple(0x050, LR_RZ_EPL, Cell3DPosition(-1, -(B2 - 2), (B2 - 1)), 3),
            Cell3DPosition(-1, -1, 2) }, // LZ_EPL3
    { LRKeyTuple(0x150, LR_RZ_EPL, Cell3DPosition(-1, -(B2 - 2), (B2 - 1)), 3),
            Cell3DPosition(-1, -1, 2) }, // LZ_EPL3
    { LRKeyTuple(0x180, LR_RZ_EPL, Cell3DPosition(-1, -(B2 - 2), (B2 - 1)), (B2 - 2)),
            Cell3DPosition(-1, -1, 1) }, // LZ_EPL4
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-(B2 - 2), -1, (B2 - 1)), 1),
            Cell3DPosition(-1, -1, 2) }, // RZ_EPL1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-(B2 - 2), -1, (B2 - 1)), 2),
            Cell3DPosition(-1, -1, 1) }, // RZ_EPL2
    { LRKeyTuple(0x005, LR_LZ_EPL, Cell3DPosition(-(B2 - 2), -1, (B2 - 1)), 3),
            Cell3DPosition(-1, -1, 2) }, // RZ_EPL3
    { LRKeyTuple(0x402, LR_LZ_EPL, Cell3DPosition(-(B2 - 2), -1, (B2 - 1)), (B2 - 2)),
            Cell3DPosition(0, 0, 1) }, // RZ_EPL4
    { LRKeyTuple(0x402, LR_LZ_EPL, Cell3DPosition(-(B2 - 2), -1, (B2 - 1)), (B2 - 2)),
          Cell3DPosition(0, 0, 1) }, // RZ_EPL4
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(-1, -1, (B2 - 1)), 1),
            Cell3DPosition(-1, -1, 2) }, //RevZ_EPL1
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(-1, -1, (B2 - 1)), 2),
            Cell3DPosition(-1, 0, 1) }, // RevZ_EPL2
    { LRKeyTuple(0xA0, LR_Z_EPL, Cell3DPosition(-1, -1, (B2 - 1)), 3),
            Cell3DPosition(-1, -1, 2) }, // RevZ_EPL3
    { LRKeyTuple(0x201, LR_Z_EPL, Cell3DPosition(-1, -1, (B2 - 1)), (B2 - 2)),
            Cell3DPosition(0, -1, 1) }, // RevZ_EPL4
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-(B2 - 2), -(B2 - 2), (B2 - 1)), 1),
            Cell3DPosition(-1, -1, 2) }, // Z_EPL1
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-(B2 - 2), -(B2 - 2), (B2 - 1)), 2),
            Cell3DPosition(0, -1, 1) }, // Z_EPL2
    { LRKeyTuple(0xA, LR_RevZ_EPL, Cell3DPosition(-(B2 - 2), -(B2 - 2), (B2 - 1)), 3),
            Cell3DPosition(-1, -1, 2) }, // Z_EPL3
    { LRKeyTuple(0x840,LR_RevZ_EPL,Cell3DPosition(-(B2 - 2), -(B2-2), (B2 - 1)), (B2 - 2)),
            Cell3DPosition(-1, 0, 1) }, // Z_EPL4

    // OPP X
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(-1, 0, 0), 1),
            Cell3DPosition(0, -1, 0) }, // OPP_X1 1
    { LRKeyTuple(0x401, LR_LZ_EPL, Cell3DPosition(-1, 0, 0), 2),
            Cell3DPosition(0, -1, 1) }, // OPP_X1 2

    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-2, 0, 0), 1),
            Cell3DPosition(-1, -1, 1) }, // OPP_X2 1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-3, 0, 0), 1),
            Cell3DPosition(-1, -1, 1) }, // OPP_X3 1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-4, 0, 0), 1),
            Cell3DPosition(-1, -1, 1) }, // OPP_X4 1
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-5, 0, 0), 1),
            Cell3DPosition(-1, -1, 1) }, // OPP_X5 1

    { LRKeyTuple(0x001, LR_LZ_EPL, Cell3DPosition(-2, 0, 0), 2),
            Cell3DPosition(0, -1, 1) }, // OPP_X2 2
    { LRKeyTuple(0x081, LR_LZ_EPL, Cell3DPosition(-3, 0, 0), 2),
            Cell3DPosition(-1, -1, 0) }, // OPP_X3 2
    { LRKeyTuple(0x081, LR_LZ_EPL, Cell3DPosition(-4, 0, 0), 2),
            Cell3DPosition(-1, -1, 1) }, // OPP_X4 2
    { LRKeyTuple(0x081, LR_LZ_EPL, Cell3DPosition(-5, 0, 0), 2),
            Cell3DPosition(-1, -1, 1) }, // OPP_X5 2

    { LRKeyTuple(0x600, LR_LZ_EPL, Cell3DPosition(-2, 0, 0), 3),
            Cell3DPosition(0, 0, -1) }, // OPP_X2 3
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(-3, 0, 0), 3),
            Cell3DPosition(0, 0, -1) }, // OPP_X3 3
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(-4, 0, 0), 3),
            Cell3DPosition(-1, 0, 0) }, // OPP_X4 3
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(-5, 0, 0), 3),
            Cell3DPosition(-1, 0, 0) }, // OPP_X5 3

    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(-4, 0, 0), 4),
            Cell3DPosition(0, 0, -1) }, // OPP_X4 4
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(-5, 0, 0), 4),
            Cell3DPosition(-1, 0, 0) }, // OPP_X5 4

    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(-5, 0, 0), 5),
            Cell3DPosition(0, 0, -1) }, // OPP_X5 5

    // OPP Y
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(0, -1, 0), 1),
            Cell3DPosition(1, 0, 0) }, // OPP_Y1 1
    { LRKeyTuple(0x802, LR_RevZ_EPL, Cell3DPosition(0, -1, 0), 2),
            Cell3DPosition(0, 0, 1) }, // OPP_Y1 2

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(0, -1, 0), 1),
      Cell3DPosition(0, -1, 1) }, // OPP_Y1 1
    { LRKeyTuple(0x002, LR_RevZ_EPL, Cell3DPosition(0, -1, 0), 2),
      Cell3DPosition(1, 1, 0) }, // OPP_Y1 2

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(0, -2, 0), 1),
            Cell3DPosition(0, -1, 1) }, // OPP_Y2 1
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(0, -3, 0), 1),
            Cell3DPosition(0, -1, 1) }, // OPP_Y3 1
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(0, -4, 0), 1),
            Cell3DPosition(0, -1, 1) }, // OPP_Y4 1
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(0, -5, 0), 1),
            Cell3DPosition(0, -1, 1) }, // OPP_Y5 1

    { LRKeyTuple(0x002, LR_RevZ_EPL, Cell3DPosition(0, -2, 0), 2),
            Cell3DPosition(0, 0, 1) }, // OPP_Y2 2
    { LRKeyTuple(0x003, LR_RevZ_EPL, Cell3DPosition(0, -3, 0), 2),
            Cell3DPosition(1, -1, 0) }, // OPP_Y3 2
    { LRKeyTuple(0x003, LR_RevZ_EPL, Cell3DPosition(0, -4, 0), 2),
            Cell3DPosition(0, -1, 1) }, // OPP_Y4 2
    { LRKeyTuple(0x003, LR_RevZ_EPL, Cell3DPosition(0, -5, 0), 2),
            Cell3DPosition(0, -1, 1) }, // OPP_Y5 2

    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(0, -2, 0), 3),
            Cell3DPosition(1, 0, -1) }, // OPP_Y2 3
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(0, -3, 0), 3),
            Cell3DPosition(1, 0, -1) }, // OPP_Y3 3
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, -4, 0), 3),
            Cell3DPosition(0, -1, 0) }, // OPP_Y4 3
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, -5, 0), 3),
            Cell3DPosition(0, -1, 0) }, // OPP_Y5 3

    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(0, -4, 0), 4),
            Cell3DPosition(1, 0, -1) }, // OPP_Y4 4
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, -5, 0), 4),
            Cell3DPosition(0, -1, 0) }, // OPP_Y5 4

    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(0, -5, 0), 5),
            Cell3DPosition(1, 0, -1) }, // OPP_Y5 5

    // New cases
    { LRKeyTuple(0xC00, LR_RZ_EPL, Cell3DPosition(-1, -1, 0), 3),
            Cell3DPosition(-1, 0, 0) }, // S_RevZ 3
    { LRKeyTuple(0x400, LR_RZ_EPL, Cell3DPosition(-1, -1, 0), 4),
            Cell3DPosition(0, 1, -1) }, // S_RevZ 4

    // Tiles with less than 4 incident branches

    // FROM REVZ ONLY

    // Climb to the righ
    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(1, 1, 0), 1),
            Cell3DPosition(-1, 0, 0) }, // S_Z 1
    { LRKeyTuple(0x280, LR_Z_EPL, Cell3DPosition(1, 1, 0), 2),
            Cell3DPosition(0, -1, 1) }, // S_Z 2
    { LRKeyTuple(0x290, LR_Z_EPL, Cell3DPosition(1, 1, 0), 2),
      Cell3DPosition(0, -1, 1) }, // S_Z 2 ALT
    // Alt: climb to the left
    { LRKeyTuple(0x100, LR_Z_EPL_ALT, Cell3DPosition(1, 1, 0), 1),
            Cell3DPosition(0 , -1, 0) }, // S_Z 1 ALT
    { LRKeyTuple(0x840, LR_Z_EPL_ALT, Cell3DPosition(1, 1, 0), 2),
            Cell3DPosition(-1, 0, 1) }, // S_Z 2 ALT
    { LRKeyTuple(0x850, LR_Z_EPL_ALT, Cell3DPosition(1, 1, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // S_Z 2 ALT 2

    { LRKeyTuple(0xC00, LR_RZ_EPL, Cell3DPosition(-1, -1, 0), 3),
            Cell3DPosition(-1, 0, 0) }, // S_RevZ 3
    { LRKeyTuple(0x400, LR_RZ_EPL, Cell3DPosition(-1, -1, 0), 4),
            Cell3DPosition(0, 1, -1) }, // S_RevZ 4

    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 1, 0), 1),
            Cell3DPosition(-1, 0, 1) }, // Y1 1
    { LRKeyTuple(0x080, LR_Z_EPL, Cell3DPosition(0, 1, 0), 2),
            Cell3DPosition(-1, -1, 0) }, // Y1 2

    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 2, 0), 1),
            Cell3DPosition(-1, 0, 1) }, // Y2 1
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 3, 0), 1),
      Cell3DPosition(-1, 0, 1) }, // Y3 1
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 4, 0), 1),
      Cell3DPosition(-1, 0, 1) }, // Y4 1
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(0, 5, 0), 1),
      Cell3DPosition(-1, 0, 1) }, // Y5 1

    { LRKeyTuple(0x080, LR_Z_EPL, Cell3DPosition(0, 2, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // Y2 2
    { LRKeyTuple(0x0c0, LR_Z_EPL, Cell3DPosition(0, 3, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // Y3 2
    { LRKeyTuple(0x0c0, LR_Z_EPL, Cell3DPosition(0, 4, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // Y4 2
    { LRKeyTuple(0x0c0, LR_Z_EPL, Cell3DPosition(0, 5, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // Y5 2

    { LRKeyTuple(0x300, LR_Z_EPL, Cell3DPosition(0, 2, 0), 3),
      Cell3DPosition(0, 1, -1) }, // Y2 3
    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(0, 3, 0), 3),
      Cell3DPosition(0, 1, -1) }, // Y3 3
    { LRKeyTuple(0x900, LR_Z_EPL, Cell3DPosition(0, 4, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y4 3
    { LRKeyTuple(0x900, LR_Z_EPL, Cell3DPosition(0, 5, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y5 3

    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(0, 4, 0), 4),
      Cell3DPosition(0, 1, -1) }, // Y4 4
    { LRKeyTuple(0x900, LR_Z_EPL, Cell3DPosition(0, 5, 0), 4),
      Cell3DPosition(0, 1, 0) }, // Y5 4

    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(0, 5, 0), 5),
      Cell3DPosition(0, 1, -1) }, // Y5 5

    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(1, 0, 0), 1),
            Cell3DPosition(0, -1, 1) }, // X1 1
    { LRKeyTuple(0x040, LR_Z_EPL, Cell3DPosition(1, 0, 0), 2),
            Cell3DPosition(-1, -1, 0) }, // X1 2

    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(2, 0, 0), 1),
            Cell3DPosition(0, -1, 1) }, // X2 1
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(3, 0, 0), 1),
            Cell3DPosition(0, -1, 1) }, // X3 1
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(4, 0, 0), 1),
            Cell3DPosition(0, -1, 1) }, // X4 1
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(5, 0, 0), 1),
            Cell3DPosition(0, -1, 1) }, // X5 1

    { LRKeyTuple(0x040, LR_Z_EPL, Cell3DPosition(2, 0, 0), 2),
          Cell3DPosition(-1, -1, 1) }, // X2 2
    { LRKeyTuple(0x0c0, LR_Z_EPL, Cell3DPosition(3, 0, 0), 2),
            Cell3DPosition(0, -1, 1) }, // X3 2
    { LRKeyTuple(0x0c0, LR_Z_EPL, Cell3DPosition(4, 0, 0), 2),
      Cell3DPosition(0, -1, 1) }, // X4 2
    { LRKeyTuple(0x0c0, LR_Z_EPL, Cell3DPosition(5, 0, 0), 2),
      Cell3DPosition(0, -1, 1) }, // X5 2

    { LRKeyTuple(0x900, LR_Z_EPL, Cell3DPosition(2, 0, 0), 3),
      Cell3DPosition(1, 0, -1) }, // X2 3
    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(3, 0, 0), 3),
      Cell3DPosition(1, 0, -1) }, // X3 3
    { LRKeyTuple(0x300, LR_Z_EPL, Cell3DPosition(4, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X4 3
    { LRKeyTuple(0x300, LR_Z_EPL, Cell3DPosition(5, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X5 3

    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(4, 0, 0), 4),
      Cell3DPosition(1, 0, -1) }, // X4 4
    { LRKeyTuple(0x300, LR_Z_EPL, Cell3DPosition(5, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X5 4

    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(5, 0, 0), 5),
      Cell3DPosition(1, 0, -1) }, // X5 5

    // END FROM REVZ ONLY

    // START OPPY BORDER //
    // R from LZ EPL
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(0, 0, 0), 1),
      Cell3DPosition(1, 0, 0) }, // R 1
    { LRKeyTuple(0x180, LR_LZ_EPL, Cell3DPosition(0, 0, 0), 2),
      Cell3DPosition(0, -1, 1) }, // R 2
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(0, 0, 0), 3),
      Cell3DPosition(0, -1, 0) }, // R 3

    // Y1 from LZ_EPL with S_LZ already in place
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(0, 1, 0), 1),
      Cell3DPosition(0, 0, 1) }, // Y 1
    { LRKeyTuple(0x080, LR_LZ_EPL, Cell3DPosition(0, 1, 0), 2),
      Cell3DPosition(1, -1, 0) }, // Y 2

    // S_RevZ from LZ EPL without support
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 1),
      Cell3DPosition(1, 0, 0) }, // S_RevZ 1
    { LRKeyTuple(0x180, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 2),
      Cell3DPosition(0, -1, 1) }, // S_RevZ 2
    { LRKeyTuple(0x180, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 3),
      Cell3DPosition(0, -1, 1) }, // S_RevZ 3
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 4),
      Cell3DPosition(-1, -1, 0) }, // S_RevZ 4
    { LRKeyTuple(0xe00, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 5),
      Cell3DPosition(0, 0, -1) }, // S_RevZ 5

    // S_RevZ from RZ EPL
    { LRKeyTuple(0x808, LR_RZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // S_RevZ 1
    { LRKeyTuple(0x800, LR_RZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 2),
      Cell3DPosition(-1, 0, 0) }, // S_RevZ 2
    { LRKeyTuple(0xd00, LR_RZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 3),
      Cell3DPosition(-1, 0, 0) }, // S_RevZ 3
    { LRKeyTuple(0x600, LR_RZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 4),
      Cell3DPosition(0, 1, -1) }, // S_RevZ 4

    // S_RevZ from LZ EPL with support
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // S_RevZ 1
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 2),
      Cell3DPosition(0, -1, 0) }, // S_RevZ 2
    { LRKeyTuple(0x700, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 3),
      Cell3DPosition(0, -1, 0) }, // S_RevZ 3
    { LRKeyTuple(0xc00, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 4),
      Cell3DPosition(1, 0, -1) }, // S_RevZ 4

    // S_RevZ from LZ EPL without support
    { LRKeyTuple(0x400, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 3),
      Cell3DPosition(1, 0, -1) }, // S_RevZ 3
    { LRKeyTuple(0x603, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 4),
      Cell3DPosition(0, -1, 0) }, // S_RevZ 4

    // START OPPX BORDER //
    // R from RZ EPL
    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(0, 0, 0), 1),
      Cell3DPosition(0, 1, 0) }, // R 1
    { LRKeyTuple(0x140, LR_RZ_EPL, Cell3DPosition(0, 0, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // R 2
    { LRKeyTuple(0x100, LR_RZ_EPL, Cell3DPosition(0, 0, 0), 3),
      Cell3DPosition(-1, 0, 0) }, // R 3

    // X1 from RZ_EPL with S_RZ already in place
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(1, 0, 0), 1),
      Cell3DPosition(0, 0, 1) }, // X1 1
    { LRKeyTuple(0x040, LR_RZ_EPL, Cell3DPosition(1, 0, 0), 2),
      Cell3DPosition(-1, 1, 0) }, // X1 2

    // S_RevZ from RZ EPL
    { LRKeyTuple(0x800, LR_RZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 1),
      Cell3DPosition(0, 1, 0) }, // S_RevZ 1
    { LRKeyTuple(0x140, LR_RZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // S_RevZ 2
    { LRKeyTuple(0x140, LR_RZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 3),
      Cell3DPosition(-1, 0, 1) }, // S_RevZ 3
    { LRKeyTuple(0x100, LR_RZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 4),
      Cell3DPosition(-1, -1, 0) }, // S_RevZ 4
    { LRKeyTuple(0xe00, LR_RZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 5),
      Cell3DPosition(0, 0, -1) }, // S_RevZ 5

    // START OPPXY CORNER //
    // R from RevZ_EPL
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(0, 0, 0), 1),
      Cell3DPosition(1, 0, -1) }, // R 1
    { LRKeyTuple(0x802, LR_RevZ_EPL, Cell3DPosition(0, 0, 0), 2),
      Cell3DPosition(1, 1, 0) }, // R 2
    { LRKeyTuple(0x048, LR_RevZ_EPL, Cell3DPosition(0, 0, 0), 3),
      Cell3DPosition(0, 1, 0) }, // R 3
    { LRKeyTuple(0x010, LR_RevZ_EPL, Cell3DPosition(0, 0, 0), 4),
      Cell3DPosition(-1, -1, 2) }, // R 4

    // S_RevZ from RevZ_EPL with only RevZ
    { LRKeyTuple(0x400, LR_RevZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 1),
      Cell3DPosition(1, 0, -1) }, // S_RevZ 1
    { LRKeyTuple(0x802, LR_RevZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 2),
      Cell3DPosition(1, 1, 0) }, // S_RevZ 2
    { LRKeyTuple(0x048, LR_RevZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 3),
      Cell3DPosition(0, 0, 1) }, // S_RevZ 3
    { LRKeyTuple(0x058, LR_RevZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 4),
      Cell3DPosition(0, 0, 1) }, // S_RevZ 4
    { LRKeyTuple(0x040, LR_RevZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 5),
      Cell3DPosition(-1, 0, 1) }, // S_RevZ 5
    { LRKeyTuple(0x100, LR_RevZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 6),
      Cell3DPosition(-1, -1, 0) }, // S_RevZ 6
    { LRKeyTuple(0xe00, LR_RevZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 7),
      Cell3DPosition(0, 0, -1) }, // S_RevZ 7

    // No B

    { LRKeyTuple(0xc00, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 5),
      Cell3DPosition(0, 0, -1) }, // S_RevZ 5
    { LRKeyTuple(0x600, LR_LZ_EPL, Cell3DPosition(-1, -1, 0), 5),
      Cell3DPosition(0, 0, -1) }, // S_RevZ 5

    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(-1, -1, 0), 1),
      Cell3DPosition(0, 1, 0) }, // S_RevZ 1
    { LRKeyTuple(0x201, LR_RevZ_EPL, Cell3DPosition(-1, -1, 0), 2),
      Cell3DPosition(0, -1, 1) }, // S_RevZ 2

    { LRKeyTuple(0x400, LR_RevZ_EPL_RIGHT, Cell3DPosition(-1, -1, 0), 1),
      Cell3DPosition(1, 0, 0) }, // S_RevZ 1
    { LRKeyTuple(0x802, LR_RevZ_EPL_RIGHT, Cell3DPosition(-1, -1, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // S_RevZ 2

    { LRKeyTuple(0x201, LR_RZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 1),
      Cell3DPosition(0, -1, 1) }, // S_RevZ 2

    { LRKeyTuple(0x400, LR_RZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 3),
      Cell3DPosition(0, 1, -1) }, // S_RevZ 3 ALT
    { LRKeyTuple(0xc03, LR_RZ_EPL_ALT, Cell3DPosition(-1, -1, 0), 4),
      Cell3DPosition(-1, 0, 0) }, // S_RevZ 4 ALT

    /// Y from RZ
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, 1, 0), 1),
      Cell3DPosition(0, 0, 1) }, // Y1 1
    { LRKeyTuple(0x040, LR_RZ_EPL, Cell3DPosition(0, 1, 0), 2),
      Cell3DPosition(-1, 1, 0) }, // Y1 2
    { LRKeyTuple(0x1c0, LR_RZ_EPL, Cell3DPosition(0, 1, 0), 3),
      Cell3DPosition(-1, 1, 0) }, // Y1 3

    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, 2, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // Y2 1
    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(0, 2, 0), 2),
      Cell3DPosition(-1, 1, 0) }, // Y2 2
    { LRKeyTuple(0xb00, LR_RZ_EPL, Cell3DPosition(0, 2, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y2 3
    { LRKeyTuple(0xb00, LR_RZ_EPL, Cell3DPosition(0, 2, 0), 4),
      Cell3DPosition(0, 1, 0) }, // Y2 4
    { LRKeyTuple(0x100, LR_RZ_EPL, Cell3DPosition(0, 2, 0), 5),
      Cell3DPosition(0, 1, -1) }, // Y2 5

    /// Bis
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, 2, 0), 4),
      Cell3DPosition(0, 1, 0) }, // Y2 4

    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, 3, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // Y3 1
    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(0, 3, 0), 2),
      Cell3DPosition(-1, 1, 0) }, // Y3 2
    { LRKeyTuple(0xb00, LR_RZ_EPL, Cell3DPosition(0, 3, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y3 3
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, 3, 0), 4),
      Cell3DPosition(0, 1, 0) }, // Y3 4
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, 3, 0), 5),
      Cell3DPosition(0, 1, 0) }, // Y3 5
    { LRKeyTuple(0x100, LR_RZ_EPL, Cell3DPosition(0, 3, 0), 6),
      Cell3DPosition(0, 1, -1) }, // Y3 6

    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, 4, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // Y4 1
    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(0, 4, 0), 2),
      Cell3DPosition(-1, 1, 0) }, // Y4 2
    { LRKeyTuple(0xb00, LR_RZ_EPL, Cell3DPosition(0, 4, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y4 3
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, 4, 0), 4),
      Cell3DPosition(0, 1, 0) }, // Y4 4
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, 4, 0), 5),
      Cell3DPosition(0, 1, 0) }, // Y4 5
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, 4, 0), 6),
      Cell3DPosition(0, 1, 0) }, // Y4 6
    { LRKeyTuple(0x100, LR_RZ_EPL, Cell3DPosition(0, 4, 0), 7),
      Cell3DPosition(0, 1, -1) }, // Y4 7

    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, 5, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // Y5 1
    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(0, 5, 0), 2),
      Cell3DPosition(-1, 1, 0) }, // Y5 2
    { LRKeyTuple(0xb00, LR_RZ_EPL, Cell3DPosition(0, 5, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y5 3
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, 5, 0), 4),
      Cell3DPosition(0, 1, 0) }, // Y5 4
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, 5, 0), 5),
      Cell3DPosition(0, 1, 0) }, // Y5 5
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, 5, 0), 6),
      Cell3DPosition(0, 1, 0) }, // Y5 6
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, 5, 0), 7),
      Cell3DPosition(0, 1, 0) }, // Y5 7
    { LRKeyTuple(0x100, LR_RZ_EPL, Cell3DPosition(0, 5, 0), 8),
      Cell3DPosition(0, 1, -1) }, // Y5 8

    // Y from RevZ
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(0, 1, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // Y1 1
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(0, 1, 0), 2),
      Cell3DPosition(1, 1, 0) }, // Y1 2
    { LRKeyTuple(0xf00, LR_RevZ_EPL, Cell3DPosition(0, 1, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y1 3
    { LRKeyTuple(0x300, LR_RevZ_EPL, Cell3DPosition(0, 1, 0), 4),
      Cell3DPosition(1, 1, -1) }, // Y1 4

    { LRKeyTuple(0x700, LR_RevZ_EPL, Cell3DPosition(0, 1, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y1 3 BIS
    { LRKeyTuple(0xd00, LR_RevZ_EPL, Cell3DPosition(0, 1, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y1 3 TER
    { LRKeyTuple(0x200, LR_RevZ_EPL, Cell3DPosition(0, 1, 0), 4),
      Cell3DPosition(1, 1, -1) }, // Y1 4 BIS/TER

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(0, 2, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // Y2 1
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(0, 2, 0), 2),
      Cell3DPosition(1, 1, 0) }, // Y2 2
    { LRKeyTuple(0xf00, LR_RevZ_EPL, Cell3DPosition(0, 2, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y2 3
    { LRKeyTuple(0x700, LR_RevZ_EPL, Cell3DPosition(0, 2, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y2 3 ALT
    { LRKeyTuple(0x700, LR_RevZ_EPL, Cell3DPosition(0, 2, 0), 4),
      Cell3DPosition(0, 1, 0) }, // Y2 4
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, 2, 0), 4),
      Cell3DPosition(0, 1, 0) }, // Y2 4 ALT
    { LRKeyTuple(0x200, LR_RevZ_EPL, Cell3DPosition(0, 2, 0), 5),
      Cell3DPosition(1, 1, -1) }, // Y2 5

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(0, 3, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // Y3 1
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(0, 3, 0), 2),
      Cell3DPosition(1, 1, 0) }, // Y3 2
    { LRKeyTuple(0x700, LR_RevZ_EPL, Cell3DPosition(0, 3, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y3 3
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, 3, 0), 4),
      Cell3DPosition(0, 1, 0) }, // Y3 4
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, 3, 0), 5),
      Cell3DPosition(0, 1, 0) }, // Y3 5
    { LRKeyTuple(0x200, LR_RevZ_EPL, Cell3DPosition(0, 3, 0), 6),
      Cell3DPosition(1, 1, -1) }, // Y3 6

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(0, 4, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // Y4 1
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(0, 4, 0), 2),
      Cell3DPosition(1, 1, 0) }, // Y4 2
    { LRKeyTuple(0x700, LR_RevZ_EPL, Cell3DPosition(0, 4, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y4 3
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, 4, 0), 4),
      Cell3DPosition(0, 1, 0) }, // Y4 4
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, 4, 0), 5),
      Cell3DPosition(0, 1, 0) }, // Y4 5
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, 4, 0), 6),
      Cell3DPosition(0, 1, 0) }, // Y4 6
    { LRKeyTuple(0x200, LR_RevZ_EPL, Cell3DPosition(0, 4, 0), 7),
      Cell3DPosition(1, 1, -1) }, // Y4 7

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(0, 5, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // Y5 1
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(0, 5, 0), 2),
      Cell3DPosition(1, 1, 0) }, // Y5 2
    { LRKeyTuple(0x700, LR_RevZ_EPL, Cell3DPosition(0, 5, 0), 3),
      Cell3DPosition(0, 1, 0) }, // Y5 3
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, 5, 0), 4),
      Cell3DPosition(0, 1, 0) }, // Y5 4
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, 5, 0), 5),
      Cell3DPosition(0, 1, 0) }, // Y5 5
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, 5, 0), 6),
      Cell3DPosition(0, 1, 0) }, // Y5 6
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(0, 5, 0), 7),
      Cell3DPosition(0, 1, 0) }, // Y5 7
    { LRKeyTuple(0x200, LR_RevZ_EPL, Cell3DPosition(0, 5, 0), 8),
      Cell3DPosition(1, 1, -1) }, // Y5 8

    // X from RevZ
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(1, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X1 1
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(1, 0, 0), 2),
      Cell3DPosition(1, 1, 0) }, // X1 2
    { LRKeyTuple(0xf00, LR_RevZ_EPL, Cell3DPosition(1, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X1 3
    { LRKeyTuple(0xd00, LR_RevZ_EPL, Cell3DPosition(1, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X1 3 No XBranch
    { LRKeyTuple(0x900, LR_RevZ_EPL, Cell3DPosition(1, 0, 0), 4),
      Cell3DPosition(1, 1, -1) }, // X1 4
    { LRKeyTuple(0x800, LR_RevZ_EPL, Cell3DPosition(1, 0, 0), 4),
      Cell3DPosition(1, 1, -1) }, // X1 4 No XBranch

    { LRKeyTuple(0x700, LR_RevZ_EPL, Cell3DPosition(1, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X1 3

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X2 1
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 2),
      Cell3DPosition(1, 1, 0) }, // X2 2
    { LRKeyTuple(0xf00, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X2 3
    { LRKeyTuple(0xd00, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X2 3 No XBranch
    { LRKeyTuple(0xd00, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X2 4
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X2 4 No XBranch
    { LRKeyTuple(0x800, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 5),
      Cell3DPosition(1, 1, -1) }, // X2 5

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X2 1
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 2),
      Cell3DPosition(1, 1, 0) }, // X2 2
    { LRKeyTuple(0xf00, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X2 3
    { LRKeyTuple(0xd00, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X2 3 No XBranch
    { LRKeyTuple(0xd00, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X2 4
    { LRKeyTuple(0x800, LR_RevZ_EPL, Cell3DPosition(2, 0, 0), 5),
      Cell3DPosition(1, 1, -1) }, // X2 5

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(3, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X3 1
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(3, 0, 0), 2),
      Cell3DPosition(1, 1, 0) }, // X3 2
    { LRKeyTuple(0xf00, LR_RevZ_EPL, Cell3DPosition(3, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X3 3
    { LRKeyTuple(0xd00, LR_RevZ_EPL, Cell3DPosition(3, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X3 3 No XBranch
    { LRKeyTuple(0xd00, LR_RevZ_EPL, Cell3DPosition(3, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X3 4
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(3, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X3 4 No XBranch
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(3, 0, 0), 5),
      Cell3DPosition(1, 0, 0) }, // X3 5
    { LRKeyTuple(0x800, LR_RevZ_EPL, Cell3DPosition(3, 0, 0), 6),
      Cell3DPosition(1, 1, -1) }, // X3 6

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(4, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X4 1
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(4, 0, 0), 2),
      Cell3DPosition(1, 1, 0) }, // X4 2
    { LRKeyTuple(0xf00, LR_RevZ_EPL, Cell3DPosition(4, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X4 3
    { LRKeyTuple(0xd00, LR_RevZ_EPL, Cell3DPosition(4, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X4 3 No XBranch
    { LRKeyTuple(0xd00, LR_RevZ_EPL, Cell3DPosition(4, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X4 4
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(4, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X4 4 No XBranch
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(4, 0, 0), 5),
      Cell3DPosition(1, 0, 0) }, // X4 5
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(4, 0, 0), 6),
      Cell3DPosition(1, 0, 0) }, // X4 6
    { LRKeyTuple(0x800, LR_RevZ_EPL, Cell3DPosition(4, 0, 0), 7),
      Cell3DPosition(1, 1, -1) }, // X4 7

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(5, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X5 1
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(5, 0, 0), 2),
      Cell3DPosition(1, 1, 0) }, // X5 2
    { LRKeyTuple(0xf00, LR_RevZ_EPL, Cell3DPosition(5, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X5 3
    { LRKeyTuple(0xd00, LR_RevZ_EPL, Cell3DPosition(5, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X5 3 No XBranch
    { LRKeyTuple(0xd00, LR_RevZ_EPL, Cell3DPosition(5, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X5 4
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(5, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X5 4 No XBranch
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(5, 0, 0), 5),
      Cell3DPosition(1, 0, 0) }, // X5 5
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(5, 0, 0), 6),
      Cell3DPosition(1, 0, 0) }, // X5 6
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(5, 0, 0), 7),
      Cell3DPosition(1, 0, 0) }, // X5 6
    { LRKeyTuple(0x800, LR_RevZ_EPL, Cell3DPosition(5, 0, 0), 8),
      Cell3DPosition(1, 1, -1) }, // X5 7

    // X from LZ
    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(1, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X1 1
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(1, 0, 0), 2),
      Cell3DPosition(1, -1, 0) }, // X1 2
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(1, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X1 3
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(1, 0, 0), 4),
      Cell3DPosition(1, 0, -1) }, // X1 4

    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(2, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X2 1
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(2, 0, 0), 2),
      Cell3DPosition(1, -1, 0) }, // X2 2
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(2, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X2 3
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(2, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X2 4
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(2, 0, 0), 5),
      Cell3DPosition(1, 0, -1) }, // X2 5

    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(3, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X3 1
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(3, 0, 0), 2),
      Cell3DPosition(1, -1, 0) }, // X3 2
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(3, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X3 3
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(3, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X3 4
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(3, 0, 0), 5),
      Cell3DPosition(1, 0, 0) }, // X3 5
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(3, 0, 0), 6),
      Cell3DPosition(1, 0, -1) }, // X3 6

    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X4 1
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 2),
      Cell3DPosition(1, -1, 0) }, // X4 2
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X4 3
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X4 4
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 5),
      Cell3DPosition(1, 0, 0) }, // X4 5
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 6),
      Cell3DPosition(1, 0, 0) }, // X4 6
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 7),
      Cell3DPosition(1, 0, -1) }, // X4 7

    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X4 1
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 2),
      Cell3DPosition(1, -1, 0) }, // X4 2
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X4 3
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X4 4
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 5),
      Cell3DPosition(1, 0, 0) }, // X4 5
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 6),
      Cell3DPosition(1, 0, 0) }, // X4 6
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 7),
      Cell3DPosition(1, 0, 0) }, // X4 7
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 8),
      Cell3DPosition(1, 0, -1) }, // X4 8

    /// BIS
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(2, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X2

    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(3, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X3 1
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(3, 0, 0), 2),
      Cell3DPosition(1, -1, 0) }, // X3 2
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(3, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X3 3
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(3, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X3 4
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(3, 0, 0), 5),
      Cell3DPosition(1, 0, 0) }, // X3 5
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(3, 0, 0), 6),
      Cell3DPosition(1, 0, -1) }, // X3 6

    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X4 1
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 2),
      Cell3DPosition(1, -1, 0) }, // X4 2
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X4 3
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X4 4
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 5),
      Cell3DPosition(1, 0, 0) }, // X4 5
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 6),
      Cell3DPosition(1, 0, 0) }, // X4 6
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(4, 0, 0), 7),
      Cell3DPosition(1, 0, -1) }, // X4 7

    { LRKeyTuple(0x220, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // X5 1
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 2),
      Cell3DPosition(1, -1, 0) }, // X5 2
    { LRKeyTuple(0xb00, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 3),
      Cell3DPosition(1, 0, 0) }, // X5 3
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 4),
      Cell3DPosition(1, 0, 0) }, // X5 4
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 5),
      Cell3DPosition(1, 0, 0) }, // X5 5
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 6),
      Cell3DPosition(1, 0, 0) }, // X5 6
    { LRKeyTuple(0x300, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 7),
      Cell3DPosition(1, 0, 0) }, // X5 7
    { LRKeyTuple(0x100, LR_LZ_EPL, Cell3DPosition(5, 0, 0), 8),
      Cell3DPosition(1, 0, -1) }, // X5 8

    // OPP_X from RevZ
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-1, 0, 0), 1),
      Cell3DPosition(-1, 0, 1) }, // OPP_X1 1
    { LRKeyTuple(0x001, LR_RevZ_EPL, Cell3DPosition(-1, 0, 0), 2),
      Cell3DPosition(1, 1, 0) }, // OPP_X1 2

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-2, 0, 0), 1),
      Cell3DPosition(-1, 0, 1) }, // OPP_X2 1
    { LRKeyTuple(0x001, LR_RevZ_EPL, Cell3DPosition(-2, 0, 0), 2),
      Cell3DPosition(0, 0, 1) }, // OPP_X2 2
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(-2, 0, 0), 3),
      Cell3DPosition(0, 1, -1) }, // OPP_X2 3

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-3, 0, 0), 1),
      Cell3DPosition(-1, 0, 1) }, // OPP_X3 1
    { LRKeyTuple(0x003, LR_RevZ_EPL, Cell3DPosition(-3, 0, 0), 2),
      Cell3DPosition(-1, 1, 0) }, // OPP_X3 2

    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-4, 0, 0), 1),
      Cell3DPosition(-1, 0, 1) }, // OPP_X4 1
    { LRKeyTuple(0x003, LR_RevZ_EPL, Cell3DPosition(-4, 0, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // OPP_X4 2
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(-4, 0, 0), 3),
      Cell3DPosition(-1, 0, 0) }, // OPP_X4 3
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(-4, 0, 0), 4),
      Cell3DPosition(0, 1, -1) }, // OPP_X4 4


    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-5, 0, 0), 1),
      Cell3DPosition(-1, 0, 1) }, // OPP_X5 1
    { LRKeyTuple(0x003, LR_RevZ_EPL, Cell3DPosition(-5, 0, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // OPP_X5 2
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(-5, 0, 0), 3),
      Cell3DPosition(-1, 0, 0) }, // OPP_X5 3
    { LRKeyTuple(0xc00, LR_RevZ_EPL, Cell3DPosition(-5, 0, 0), 4),
      Cell3DPosition(-1, 0, 0) }, // OPP_X5 4
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(-5, 0, 0), 5),
      Cell3DPosition(0, 1, -1) }, // OPP_X5 5

    // OPP_X ACTIVATION BELOW

    // R from Z_EPL with OPPX Activation
    { LRKeyTuple(0x100, LR_Z_EPL_ALT, Cell3DPosition(0, 0, 0), 1),
      Cell3DPosition(-1, 0, 0) }, // R 1
    { LRKeyTuple(0x280, LR_Z_EPL_ALT, Cell3DPosition(0, 0, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // R 2
    { LRKeyTuple(0x300, LR_Z_EPL_ALT, Cell3DPosition(0, 0, 0), 3),
      Cell3DPosition(0, -1, 0) }, // R 3

    // R from Z_EPL with OPPY Activation
    { LRKeyTuple(0x140, LR_Z_EPL, Cell3DPosition(0, 0, 0), 3),
      Cell3DPosition(0, -1, 0) }, // R 3
    { LRKeyTuple(0x900, LR_Z_EPL, Cell3DPosition(0, 0, 0), 4),
      Cell3DPosition(-1, 0, 0) }, // R 4

    // S_RZ with XBranch already in place
    { LRKeyTuple(0x800, LR_RZ_EPL_ALT, Cell3DPosition(1, -1, 0), 1),
      Cell3DPosition(-1,0, 0) }, // S_RZ 1
    { LRKeyTuple(0x402, LR_RZ_EPL_ALT, Cell3DPosition(1, -1, 0), 2),
      Cell3DPosition(0, 0, 1) }, // S_RZ 2

    // OPP_Y1 from RZ
    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(0, -1, 0), 1),
      Cell3DPosition(-1, 0, 0) }, // OPP_Y1 1
    { LRKeyTuple(0x402, LR_RZ_EPL, Cell3DPosition(0, -1, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // OPP_Y1 2

    // Y1 from LZ
    { LRKeyTuple(0x200, LR_LZ_EPL, Cell3DPosition(0, 1, 0), 1),
      Cell3DPosition(1, 0, 0) }, // Y1 1
    { LRKeyTuple(0x180, LR_LZ_EPL, Cell3DPosition(0, 1, 0), 2),
      Cell3DPosition(0, -1, 1) }, // Y1 2

    // S_LZ from RevZ_EPL
    { LRKeyTuple(0x404, LR_RevZ_EPL, Cell3DPosition(-1, 1, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // S_LZ 1
    { LRKeyTuple(0x400, LR_RevZ_EPL, Cell3DPosition(-1, 1, 0), 2),
      Cell3DPosition(0, 1, 0) }, // S_LZ 2
    { LRKeyTuple(0x600, LR_RevZ_EPL, Cell3DPosition(-1, 1, 0), 3),
      Cell3DPosition(0, 1, 0) }, // S_LZ 3
    { LRKeyTuple(0x200, LR_RevZ_EPL, Cell3DPosition(-1, 1, 0), 3),
      Cell3DPosition(1, 1, -1) }, // S_LZ 3 BIS
    { LRKeyTuple(0x200, LR_RevZ_EPL, Cell3DPosition(-1, 1, 0), 4),
      Cell3DPosition(1, 1, -1) }, // S_LZ 4
    { LRKeyTuple(0x681, LR_RevZ_EPL, Cell3DPosition(-1, 1, 0), 4),
      Cell3DPosition(0, 1, 0) }, // S_LZ 4 BIS

    // S_RZ from Z_EPL
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(1, -1, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // S_RZ 1
    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(1, -1, 0), 2),
      Cell3DPosition(0, -1, 0) }, // S_RZ 2
    { LRKeyTuple(0xb00, LR_Z_EPL, Cell3DPosition(1, -1, 0), 3),
      Cell3DPosition(0, -1, 0) }, // S_RZ 3
    { LRKeyTuple(0xc00, LR_Z_EPL, Cell3DPosition(1, -1, 0), 4),
      Cell3DPosition(0, 0, -1) }, // S_RZ 4

    { LRKeyTuple(0x900, LR_Z_EPL, Cell3DPosition(1, -1, 0), 3),
      Cell3DPosition(0, -1, 0) }, // S_RZ 3
    { LRKeyTuple(0x800, LR_Z_EPL, Cell3DPosition(1, -1, 0), 4),
      Cell3DPosition(0, 0, -1) }, // S_RZ 4

    // OPP Y ACTIVATION BELOW

    // S_LZ from Z_EPL
    { LRKeyTuple(0x110, LR_Z_EPL, Cell3DPosition(-1, 1, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // S_LZ 1
    { LRKeyTuple(0x100, LR_Z_EPL, Cell3DPosition(-1, 1, 0), 2),
      Cell3DPosition(-1, 0, 0) }, // S_LZ 2
    { LRKeyTuple(0xb00, LR_Z_EPL, Cell3DPosition(-1, 1, 0), 3),
      Cell3DPosition(-1, -1, 0) }, // S_LZ 3
    { LRKeyTuple(0x700, LR_Z_EPL, Cell3DPosition(-1, 1, 0), 4),
      Cell3DPosition(0, 1, -1) }, // S_LZ 4
    { LRKeyTuple(0x600, LR_Z_EPL, Cell3DPosition(-1, 1, 0), 4),
      Cell3DPosition(0, 1, -1) }, // S_LZ 4 BIS

    { LRKeyTuple(0x405, LR_LZ_EPL, Cell3DPosition(-1, 0, 0), 2),
      Cell3DPosition(0, -1, 1) }, // OPP_X1 2

    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(1, 0, 0), 1),
      Cell3DPosition(0, 1, 0) }, // X1 1 from RZ
    { LRKeyTuple(0x140, LR_RZ_EPL, Cell3DPosition(1, 0, 0), 2),
      Cell3DPosition(-1, 0, 1) }, // X1 2 from RZ_R_EPL

    { LRKeyTuple(0x148, LR_RZ_EPL, Cell3DPosition(1, -1, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // S_RZ 2

    // 3 IB Corner LZ/Z/RZ

    // OPP_Y from RZ with support
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -1, 0), 1),
      Cell3DPosition(-1, -1, 1) }, // OPP_Y1 1
    { LRKeyTuple(0x002, LR_RZ_EPL, Cell3DPosition(0, -1, 0), 2),
      Cell3DPosition(-1, 1, 0) }, // OPP_Y1 2

    // OPP_Y from RZ with support
    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -2, 0), 1),
      Cell3DPosition(-1, -1, 2) }, // OPP_Y2 1
    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(0, -2, 0), 2),
      Cell3DPosition(-1, 0, 0) }, // OPP_Y2 2
    { LRKeyTuple(0xc00, LR_RZ_EPL, Cell3DPosition(0, -2, 0), 3),
      Cell3DPosition(0, 0, -1) }, // OPP_Y2 3

    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -3, 0), 1),
      Cell3DPosition(-1, -1, 1) }, // OPP_Y3 1
    { LRKeyTuple(0x042, LR_RZ_EPL, Cell3DPosition(0, -3, 0), 2),
      Cell3DPosition(-1, -1, 0) }, // OPP_Y3 2

    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -4, 0), 1),
      Cell3DPosition(-1, -1, 1) }, // OPP_Y4 1
    { LRKeyTuple(0x042, LR_RZ_EPL, Cell3DPosition(0, -4, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // OPP_Y4 2
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, -4, 0), 3),
      Cell3DPosition(0, -1, 0) }, // OPP_Y4 3
    { LRKeyTuple(0x800, LR_RZ_EPL, Cell3DPosition(0, -4, 0), 4),
      Cell3DPosition(0, 0, -1) }, // OPP_Y4 4

    { LRKeyTuple(0x808, LR_RZ_EPL, Cell3DPosition(0, -5, 0), 1),
      Cell3DPosition(-1, -1, 1) }, // OPP_Y5 1
    { LRKeyTuple(0x042, LR_RZ_EPL, Cell3DPosition(0, -5, 0), 2),
      Cell3DPosition(-1, -1, 1) }, // OPP_Y5 2
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, -5, 0), 3),
      Cell3DPosition(0, -1, 0) }, // OPP_Y5 3
    { LRKeyTuple(0x900, LR_RZ_EPL, Cell3DPosition(0, -5, 0), 4),
      Cell3DPosition(0, -1, 0) }, // OPP_Y5 4
    { LRKeyTuple(0x800 , LR_RZ_EPL, Cell3DPosition(0, -5, 0), 5),
      Cell3DPosition(0, 0, -1) }, // OPP_Y5 5


    { LRKeyTuple(0x200, LR_Z_EPL_ALT, Cell3DPosition(0, 0, 0), 3),
      Cell3DPosition(0, -1, 0) }, // R 3
    { LRKeyTuple(0x800, LR_Z_EPL, Cell3DPosition(0, 0, 0), 4),
      Cell3DPosition(-1, 0, 0) }, // R 4


    // half sphere fix
    { LRKeyTuple(0x200, LR_LZ_EPL_ALT, Cell3DPosition(-1, 1, 0), 1),
      Cell3DPosition(0, -1, 0) }, // S_LZ 1
    { LRKeyTuple(0x401, LR_LZ_EPL_ALT, Cell3DPosition(-1, 1, 0), 2),
      Cell3DPosition(0, 0, 1) }, // S_LZ 2

    // teubi
    { LRKeyTuple(0x421, LR_LZ_EPL_ALT, Cell3DPosition(-1, 1, 0), 2),
      Cell3DPosition(0, 0, 1) }, // S_LZ 2
};

/**
 * Search for the next action among the local rules library
 * @param localNeighborhood a bitset representing the local neighborhood
 *  of module at position pos
 * @param pos position of the rule matching module
 * @param tPos final target position of the module awaiting action
 * @param tileRootPos the position of the nearest tile root, for normalizing the coordinates
 * @param nextPos next position that the moving module has occupy
 * @return the matched next position if there is one, pos otherwise (meaning no movement)
 */
inline static bool matchLocalRules(const std::bitset<12>& localNeighborhood,
                                   const Cell3DPosition& pos,
                                   const Cell3DPosition& tPos,
                                   const Cell3DPosition& tileRootPos,
                                   const short step,
                                   const short EPL,
                                   Cell3DPosition& nextPos,
                                   bool debug=false) {
    auto match = localMotionRules.find(LRKeyTuple(localNeighborhood, EPL,
                                                  tPos - tileRootPos, step));

    if (match != localMotionRules.end()) {
        nextPos =  match->second + pos;

        if (debug)
            cout << match->second << endl;
    } else {
        if (debug) {
            cerr << "{ " << localNeighborhood << "("
                 << int_to_hex_str((int)localNeighborhood.to_ulong(), 3) << ")"
                 << ", " << EPL << ", " << tPos - tileRootPos
                 << ", " << step << " }" << " -> ";
            cerr << "NO MATCH" << endl;
        }
    }

    return match != localMotionRules.end();
}

#endif /* MESHASSEMBLYLOCALRULES_HPP__ */
