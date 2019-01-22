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

class LRKeyTuple {
    const std::bitset<12> lnBitset;
    const Cell3DPosition tPos;
    const short step;
public:
    constexpr LRKeyTuple(int ln, const Cell3DPosition& _tPos, const short _step)
        : lnBitset(bitset<12>(ln)), tPos(_tPos), step(_step) {}

    constexpr LRKeyTuple(const bitset<12>& _lnBitset, const Cell3DPosition& _tPos,
                         const short _step)
        : lnBitset(bitset<12>(_lnBitset)), tPos(_tPos), step(_step) {}
    
    bool operator<(const LRKeyTuple &lrkt) const {
        if (lnBitset.to_ulong() < lrkt.lnBitset.to_ulong()) return true;
        else if (lnBitset.to_ulong() > lrkt.lnBitset.to_ulong()) return false;
        else if (step < lrkt.step) return true;
        else if (step > lrkt.step) return false;
        else return tPos < lrkt.tPos;
    }

    bool operator==(const LRKeyTuple &lrkt) const {       
        return lnBitset.to_ulong() == lrkt.lnBitset.to_ulong()
            and tPos == lrkt.tPos
            and step == lrkt.step;
    }
};

// ORIGINAL RULES
// static const std::map <const LRKeyTuple, const Cell3DPosition> localMotionRules = 
// {
//     // Corner XY Cases
//     { LRKeyTuple(0x400, Cell3DPosition(-1, -1, 0), 3), Cell3DPosition(0, 1, -1) }, // S_RevZ 3
//     { LRKeyTuple(0xC03, Cell3DPosition(-1, -1, 0), 4), Cell3DPosition(-1, 0, 0) }, // S_RevZ 4

//     // Border OppX Cases
//     { LRKeyTuple(0x380, Cell3DPosition(0, 0, 2), 3), Cell3DPosition(0, -1, 1) }, // Z2 3

//     // Border OppY Cases
//     { LRKeyTuple(0x940, Cell3DPosition(0, -2, 2), 3), Cell3DPosition(-1, -1, 1) }, // RZ2 3    

//     // Pyramid Corner cases
//     { LRKeyTuple(0x100, Cell3DPosition(1, 1, 0), 3), Cell3DPosition(1, 0, -1) }, // S_Z 3
//     { LRKeyTuple(0x3C0, Cell3DPosition(1, 1, 0), 4), Cell3DPosition(1, 0, 0) }, // S_Z 4    
    
//     // Normal Cases
//     { LRKeyTuple(0x840, Cell3DPosition(0, 0, 0), 1), Cell3DPosition(-1, 0, 1) }, // R 1
//     { LRKeyTuple(0x100, Cell3DPosition(0, 0, 0), 2), Cell3DPosition(-1, -1, 0) }, // R 2
//     { LRKeyTuple(0x140, Cell3DPosition(1, -1, 0), 1), Cell3DPosition(-1, -1, 1) }, // S_RZ 1
//     { LRKeyTuple(0x180, Cell3DPosition(-1, 1, 0), 1), Cell3DPosition(-1, -1, 1) }, // S_LZ 1
//     { LRKeyTuple(0x840, Cell3DPosition(1, 0, 0), 1), Cell3DPosition(-1, -1, 1) }, // X1 1
//     { LRKeyTuple(0x280, Cell3DPosition(0, 1, 0), 1), Cell3DPosition(-1, -1, 1) }, // Y1 1
//     { LRKeyTuple(0x220, Cell3DPosition(1, 1, 0), 1), Cell3DPosition(-1, -1, 2) }, // S_Z 1
//     { LRKeyTuple(0x808, Cell3DPosition(-1, -1, 0), 1), Cell3DPosition(-1, -1, 2) }, // S_RevZ 1
//     { LRKeyTuple(0x200, Cell3DPosition(1, 1, 0), 2), Cell3DPosition(1, 0, 0) }, // S_Z 2
//     { LRKeyTuple(0x800, Cell3DPosition(-1, -1, 0), 2), Cell3DPosition(-1, 0, 0) }, // S_RevZ 2
//     { LRKeyTuple(0x300, Cell3DPosition(1, 1, 0), 3), Cell3DPosition(1, 0, 0) }, // S_Z 3
//     { LRKeyTuple(0xD00, Cell3DPosition(-1, -1, 0), 3), Cell3DPosition(-1, 0, 0) }, // S_RevZ 3
//     { LRKeyTuple(0x808, Cell3DPosition(2, 0, 0), 1), Cell3DPosition(0, 0, 1) }, // X2 1
//     { LRKeyTuple(0x220, Cell3DPosition(0, 2, 0), 1), Cell3DPosition(0, 0, 1) }, // Y2 1
//     { LRKeyTuple(0x100, Cell3DPosition(1, 1, 0), 4), Cell3DPosition(1, 0, -1) }, // S_Z 4
//     { LRKeyTuple(0x600, Cell3DPosition(-1, -1, 0), 4), Cell3DPosition(0, 1, -1) }, // S_RevZ 4
//     { LRKeyTuple(0x040, Cell3DPosition(2, 0, 0), 2), Cell3DPosition(-1, 0, 1) }, // X2 2
//     { LRKeyTuple(0x080, Cell3DPosition(0, 2, 0), 2), Cell3DPosition(0, -1, 1) }, // Y2 2
//     { LRKeyTuple(0x900, Cell3DPosition(2, 0, 0), 3), Cell3DPosition(1, 1, -1) }, // X2 3
//     { LRKeyTuple(0x300, Cell3DPosition(0, 2, 0), 3), Cell3DPosition(1, 1, -1) }, // Y2 3
//     { LRKeyTuple(0x110, Cell3DPosition(0, 0, 1), 1), Cell3DPosition(-1, -1, 2) }, // Z1 1
//     { LRKeyTuple(0x404, Cell3DPosition(-1, -1, 1), 1), Cell3DPosition(-1, -1, 2) }, // RevZ1 1
//     { LRKeyTuple(0x808, Cell3DPosition(3, 0, 0), 1), Cell3DPosition(0, 0, 1) }, // X3 1
//     { LRKeyTuple(0x220, Cell3DPosition(0, 3, 0), 1), Cell3DPosition(0, 0, 1) }, // Y3 1
//     { LRKeyTuple(0x100, Cell3DPosition(0, 0, 1), 2), Cell3DPosition(-1, -1, 0) }, // Z1 2
//     { LRKeyTuple(0x400, Cell3DPosition(-1, -1, 1), 2), Cell3DPosition(1, 1, 0) }, // RevZ1 2
//     { LRKeyTuple(0x042, Cell3DPosition(3, 0, 0), 2), Cell3DPosition(1, 1, 0) }, // X3 2
//     { LRKeyTuple(0x081, Cell3DPosition(0, 3, 0), 2), Cell3DPosition(1, 1, 0) }, // Y3 2
//     { LRKeyTuple(0x110, Cell3DPosition(0, 0, 2), 1), Cell3DPosition(-1, -1, 2) }, // Z2 1
//     { LRKeyTuple(0x404, Cell3DPosition(-2, -2, 2), 1), Cell3DPosition(-1, -1, 2) }, // RevZ2 1
//     { LRKeyTuple(0x808, Cell3DPosition(4, 0, 0), 1), Cell3DPosition(0, 0, 1) }, // X4 1
//     { LRKeyTuple(0x220, Cell3DPosition(0, 4, 0), 1), Cell3DPosition(0, 0, 1) }, // Y4 1
//     { LRKeyTuple(0x100, Cell3DPosition(0, 0, 2), 2), Cell3DPosition(-1, 0, 0) }, // Z2 2
//     { LRKeyTuple(0x400, Cell3DPosition(-2, -2, 2), 2), Cell3DPosition(1, 0, 0) }, // RevZ2 2
//     { LRKeyTuple(0x042, Cell3DPosition(4, 0, 0), 2), Cell3DPosition(0, 0, 1) }, // X4 2
//     { LRKeyTuple(0x081, Cell3DPosition(0, 4, 0), 2), Cell3DPosition(0, 0, 1) }, // Y4 2
//     { LRKeyTuple(0x110, Cell3DPosition(0, 0, 3), 1), Cell3DPosition(-1, -1, 2) }, // Z3 1
//     { LRKeyTuple(0x404, Cell3DPosition(-3, -3, 3), 1), Cell3DPosition(-1, -1, 2) }, // RevZ3 1
//     { LRKeyTuple(0x808, Cell3DPosition(5, 0, 0), 1), Cell3DPosition(0, 0, 1) }, // X5 1
//     { LRKeyTuple(0x220, Cell3DPosition(0, 5, 0), 1), Cell3DPosition(0, 0, 1) }, // Y5 1
//     { LRKeyTuple(0xB80, Cell3DPosition(0, 0, 2), 3), Cell3DPosition(0, -1, 1) }, // Z2 3
//     { LRKeyTuple(0xE02, Cell3DPosition(-2, -2, 2), 3), Cell3DPosition(-1, 0, 1) }, // RevZ2 3
//     { LRKeyTuple(0xC00, Cell3DPosition(4, 0, 0), 3), Cell3DPosition(1, 0, 0) }, // X4 3
//     { LRKeyTuple(0x600, Cell3DPosition(0, 4, 0), 3), Cell3DPosition(0, 1, 0) }, // Y4 3
//     { LRKeyTuple(0x800, Cell3DPosition(4, 0, 0), 4), Cell3DPosition(1, 1, -1) }, // X4 4
//     { LRKeyTuple(0x200, Cell3DPosition(0, 4, 0), 4), Cell3DPosition(1, 1, -1) }, // Y4 4
//     { LRKeyTuple(0x110, Cell3DPosition(0, 0, 3), 2), Cell3DPosition(-1, -1, 2) }, // Z3 2
//     { LRKeyTuple(0x404, Cell3DPosition(-3, -3, 3), 2), Cell3DPosition(-1, -1, 2) }, // RevZ3 2
//     { LRKeyTuple(0x042, Cell3DPosition(5, 0, 0), 2), Cell3DPosition(0, 0, 1) }, // X5 2
//     { LRKeyTuple(0x081, Cell3DPosition(0, 5, 0), 2), Cell3DPosition(0, 0, 1) }, // Y5 2
//     { LRKeyTuple(0x110, Cell3DPosition(0, 0, 4), 1), Cell3DPosition(-1, -1, 2) }, // Z4 1
//     { LRKeyTuple(0x404, Cell3DPosition(-4, -4, 4), 1), Cell3DPosition(-1, -1, 2) }, // RevZ4 1
//     { LRKeyTuple(0x808, Cell3DPosition(0, -1, 1), 1), Cell3DPosition(-1, -1, 2) }, // RZ1 1
//     { LRKeyTuple(0x220, Cell3DPosition(-1, 0, 1), 1), Cell3DPosition(-1, -1, 2) }, // LZ1 1
//     { LRKeyTuple(0xC00, Cell3DPosition(5, 0, 0), 3), Cell3DPosition(1, 0, 0) }, // X5 3
//     { LRKeyTuple(0x600, Cell3DPosition(0, 5, 0), 3), Cell3DPosition(0, 1, 0) }, // Y5 3
//     { LRKeyTuple(0x110, Cell3DPosition(0, 0, 4), 2), Cell3DPosition(-1, 0, 1) }, // Z4 2
//     { LRKeyTuple(0x404, Cell3DPosition(-4, -4, 4), 2), Cell3DPosition(0, -1, 1) }, // RevZ4 2
//     { LRKeyTuple(0x800, Cell3DPosition(0, -1, 1), 2), Cell3DPosition(-1, 1, 0) }, // RZ1 2
//     { LRKeyTuple(0x200, Cell3DPosition(-1, 0, 1), 2), Cell3DPosition(1, -1, 0) }, // LZ1 2
//     { LRKeyTuple(0xC00, Cell3DPosition(5, 0, 0), 4), Cell3DPosition(1, 1, 0) }, // X5 4
//     { LRKeyTuple(0x600, Cell3DPosition(0, 5, 0), 4), Cell3DPosition(1, 1, 0) }, // Y5 4
//     { LRKeyTuple(0xA0, Cell3DPosition(0, 0, 4), 3), Cell3DPosition(-1, -1, 2) }, // Z4 3
//     { LRKeyTuple(0xA, Cell3DPosition(-4, -4, 4), 3), Cell3DPosition(-1, -1, 2) }, // RevZ4 3
//     { LRKeyTuple(0x100, Cell3DPosition(5, 0, 0), 5), Cell3DPosition(1, 0, -1) }, // X5 5
//     { LRKeyTuple(0x100, Cell3DPosition(0, 5, 0), 5), Cell3DPosition(0, 1, -1) }, // Y5 5
//     { LRKeyTuple(0x110, Cell3DPosition(0, 0, 5), 1), Cell3DPosition(-1, -1, 2) }, // Z5 1
//     { LRKeyTuple(0x404, Cell3DPosition(-5, -5, 5), 1), Cell3DPosition(-1, -1, 2) }, // RevZ5 1
//     { LRKeyTuple(0x808, Cell3DPosition(0, -2, 2), 1), Cell3DPosition(-1, -1, 2) }, // RZ2 1
//     { LRKeyTuple(0x220, Cell3DPosition(-2, 0, 2), 1), Cell3DPosition(-1, -1, 2) }, // LZ2 1
//     { LRKeyTuple(0x200, Cell3DPosition(0, 0, 4), 4), Cell3DPosition(1, 0, 0) }, // Z4 4
//     { LRKeyTuple(0x800, Cell3DPosition(-4, -4, 4), 4), Cell3DPosition(-1, 0, 0) }, // RevZ4 4
//     { LRKeyTuple(0x110, Cell3DPosition(0, 0, 5), 2), Cell3DPosition(-1, 0, 1) }, // Z5 2
//     { LRKeyTuple(0x404, Cell3DPosition(-5, -5, 5), 2), Cell3DPosition(0, -1, 1) }, // RevZ5 2
//     { LRKeyTuple(0x800, Cell3DPosition(0, -2, 2), 2), Cell3DPosition(0, 1, 0) }, // RZ2 2
//     { LRKeyTuple(0x200, Cell3DPosition(-2, 0, 2), 2), Cell3DPosition(0, -1, 0) }, // LZ2 2
//     { LRKeyTuple(0xA0, Cell3DPosition(0, 0, 5), 3), Cell3DPosition(0, 0, 1) }, // Z5 3
//     { LRKeyTuple(0xA, Cell3DPosition(-5, -5, 5), 3), Cell3DPosition(-1, -1, 2) }, // RevZ5 3
//     { LRKeyTuple(0xD40, Cell3DPosition(0, -2, 2), 3), Cell3DPosition(-1, -1, 1) }, // RZ2 3
//     { LRKeyTuple(0x701, Cell3DPosition(-2, 0, 2), 3), Cell3DPosition(0, 0, 1) }, // LZ2 3
//     { LRKeyTuple(0x808, Cell3DPosition(0, -3, 3), 1), Cell3DPosition(-1, -1, 2) }, // RZ3 1
//     { LRKeyTuple(0x220, Cell3DPosition(-3, 0, 3), 1), Cell3DPosition(-1, -1, 2) }, // LZ3 1
//     { LRKeyTuple(0xA0, Cell3DPosition(0, 0, 5), 4), Cell3DPosition(-1, -1, 2) }, // Z5 4
//     { LRKeyTuple(0x840, Cell3DPosition(-5, -5, 5), 4), Cell3DPosition(-1, -1, 1) }, // RevZ5 4
//     { LRKeyTuple(0x808, Cell3DPosition(0, -3, 3), 2), Cell3DPosition(-1, -1, 2) }, // RZ3 2
//     { LRKeyTuple(0x220, Cell3DPosition(-3, 0, 3), 2), Cell3DPosition(-1, -1, 2) }, // LZ3 2
//     { LRKeyTuple(0x200, Cell3DPosition(0, 0, 5), 5), Cell3DPosition(1, 0, 0) }, // Z5 5
//     { LRKeyTuple(0x800, Cell3DPosition(-5, -5, 5), 5), Cell3DPosition(-1, 0, 0) }, // RevZ5 5
//     { LRKeyTuple(0x808, Cell3DPosition(0, -4, 4), 1), Cell3DPosition(-1, -1, 2) }, // RZ4 1
//     { LRKeyTuple(0x220, Cell3DPosition(-4, 0, 4), 1), Cell3DPosition(-1, -1, 2) }, // LZ4 1
//     { LRKeyTuple(0x808, Cell3DPosition(0, -4, 4), 2), Cell3DPosition(0, 0, 1) }, // RZ4 2
//     { LRKeyTuple(0x220, Cell3DPosition(-4, 0, 4), 2), Cell3DPosition(-1, -1, 1) }, // LZ4 2
//     { LRKeyTuple(0x050, Cell3DPosition(0, -4, 4), 3), Cell3DPosition(0, -1, 1) }, // RZ4 3
//     { LRKeyTuple(0x005, Cell3DPosition(-4, 0, 4), 3), Cell3DPosition(-1, 0, 1) }, // LZ4 3
//     { LRKeyTuple(0x808, Cell3DPosition(0, -5, 5), 1), Cell3DPosition(-1, -1, 2) }, // RZ5 1
//     { LRKeyTuple(0x220, Cell3DPosition(-5, 0, 5), 1), Cell3DPosition(-1, -1, 2) }, // LZ5 1
//     { LRKeyTuple(0x040, Cell3DPosition(0, -4, 4), 4), Cell3DPosition(-1, -1, 1) }, // RZ4 4
//     { LRKeyTuple(0x001, Cell3DPosition(-4, 0, 4), 4), Cell3DPosition(0, 0, 1) }, // LZ4 4
//     { LRKeyTuple(0x808, Cell3DPosition(0, -5, 5), 2), Cell3DPosition(0, 0, 1) }, // RZ5 2
//     { LRKeyTuple(0x220, Cell3DPosition(-5, 0, 5), 2), Cell3DPosition(-1, -1, 1) }, // LZ5 2
//     { LRKeyTuple(0x050, Cell3DPosition(0, -5, 5), 3), Cell3DPosition(0, -1, 1) }, // RZ5 3
//     { LRKeyTuple(0x005, Cell3DPosition(-5, 0, 5), 3), Cell3DPosition(-1, 0, 1) }, // LZ5 3
//     { LRKeyTuple(0x050, Cell3DPosition(0, -5, 5), 4), Cell3DPosition(-1, -1, 2) }, // RZ5 4
//     { LRKeyTuple(0x005, Cell3DPosition(-5, 0, 5), 4), Cell3DPosition(-1, -1, 2) }, // LZ5 4
//     { LRKeyTuple(0x100, Cell3DPosition(0, -5, 5), 5), Cell3DPosition(0, -1, 0) }, // RZ5 5
//     { LRKeyTuple(0x400, Cell3DPosition(-5, 0, 5), 5), Cell3DPosition(0, 1, 0) }, // LZ5 5

//     // Vertical Branches Climbing Rules
//     { LRKeyTuple(0x404, Cell3DPosition(-4, -5, 5), 1), Cell3DPosition(-1, -1, 2) }, //Z_R_EPL 1
//     { LRKeyTuple(0x404, Cell3DPosition(-4, -5, 5), 2), Cell3DPosition(0, -1, 1) }, // Z_R_EPL 2
//     { LRKeyTuple(0xA, Cell3DPosition(-4, -5, 5), 3), Cell3DPosition(-1, -1, 2) }, // Z_R_EPL 3
//     { LRKeyTuple(0x840, Cell3DPosition(-4, -5, 5), 4), Cell3DPosition(-1, -1, 1) }, //Z_R_EPL 4
//     { LRKeyTuple(0x220, Cell3DPosition(-4, 0, 5), 1), Cell3DPosition(-1, -1, 2) }, // RZ_R_EPL1
//     { LRKeyTuple(0x220, Cell3DPosition(-4, 0, 5), 2), Cell3DPosition(0, 0, 1) }, // RZ_R_EPL2
//     { LRKeyTuple(0x090, Cell3DPosition(-4, 0, 5), 3), Cell3DPosition(-1, 0, 1) }, // RZ_R_EPL3
//     { LRKeyTuple(0x090, Cell3DPosition(-4, 0, 5), 4), Cell3DPosition(-1, -1, 2) }, // RZ_R_EPL4
//     { LRKeyTuple(0x808, Cell3DPosition(0, -4, 5), 1), Cell3DPosition(-1, -1, 2) }, // LZ_R_EPL1
//     { LRKeyTuple(0x808, Cell3DPosition(0, -4, 5), 2), Cell3DPosition(0, 0, 1) }, // LZ_R_EPL2
//     { LRKeyTuple(0x050, Cell3DPosition(0, -4, 5), 3), Cell3DPosition(0, -1, 1) }, // LZ_R_EPL3
//     { LRKeyTuple(0x050, Cell3DPosition(0, -4, 5), 4), Cell3DPosition(-1, -1, 2) }, // LZ_R_EPL4
    

//     { LRKeyTuple(0x404, Cell3DPosition(-5, -4, 5), 1), Cell3DPosition(-1, -1, 2) }, // Z_L_EPL1
//     { LRKeyTuple(0x404, Cell3DPosition(-5, -4, 5), 2), Cell3DPosition(-1, 0, 1) }, // Z_L_EPL2
//     { LRKeyTuple(0x021, Cell3DPosition(-5, -4, 5), 3), Cell3DPosition(-1, -1, 1) }, // Z_L_EPL3
//     { LRKeyTuple(0x021, Cell3DPosition(-5, -4, 5), 4), Cell3DPosition(-1, -1, 2) }, // Z_L_EPL4
//     // next 4 rules might be superfluous
//     { LRKeyTuple(0x404, Cell3DPosition(-4, -5, 5), 1), Cell3DPosition(-1, -1, 2) }, // Z_R_EPL1
//     { LRKeyTuple(0x404, Cell3DPosition(-4, -5, 5), 2), Cell3DPosition(0, -1, 1) }, // Z_R_EPL2
//     { LRKeyTuple(0xA, Cell3DPosition(-4, -5, 5), 3), Cell3DPosition(-1, -1, 1) }, // Z_R_EPL3
//     { LRKeyTuple(0xA, Cell3DPosition(-4, -5, 5), 4), Cell3DPosition(-1, -1, 2) }, // Z_R_EPL4  

//     { LRKeyTuple(0x808, Cell3DPosition(-1, -4, 5), 1), Cell3DPosition(-1, -1, 2) }, // LZ_EPL1
//     { LRKeyTuple(0x808, Cell3DPosition(-1, -4, 5), 2), Cell3DPosition(0, 0, 1) }, // LZ_EPL2
//     { LRKeyTuple(0x050, Cell3DPosition(-1, -4, 5), 3), Cell3DPosition(-1, -1, 2) }, // LZ_EPL3
//     { LRKeyTuple(0x180, Cell3DPosition(-1, -4, 5), 4), Cell3DPosition(-1, -1, 1) }, // LZ_EPL4
//     { LRKeyTuple(0x220, Cell3DPosition(-4, -1, 5), 1), Cell3DPosition(-1, -1, 2) }, // RZ_EPL1
//     { LRKeyTuple(0x220, Cell3DPosition(-4, -1, 5), 2), Cell3DPosition(-1, -1, 1) }, // RZ_EPL2
//     { LRKeyTuple(0x005, Cell3DPosition(-4, -1, 5), 3), Cell3DPosition(-1, -1, 2) }, // RZ_EPL3
//     { LRKeyTuple(0x402, Cell3DPosition(-4, -1, 5), 4), Cell3DPosition(0, 0, 1) }, // RZ_EPL4
//     { LRKeyTuple(0x110, Cell3DPosition(-1, -1, 5), 1), Cell3DPosition(-1, -1, 2) }, //RevZ_EPL1
//     { LRKeyTuple(0x110, Cell3DPosition(-1, -1, 5), 2), Cell3DPosition(-1, 0, 1) }, // RevZ_EPL2
//     { LRKeyTuple(0xA0, Cell3DPosition(-1, -1, 5), 3), Cell3DPosition(-1, -1, 2) }, // RevZ_EPL3
//     { LRKeyTuple(0x201, Cell3DPosition(-1, -1, 5), 4), Cell3DPosition(0, -1, 1) }, // RevZ_EPL4
//     { LRKeyTuple(0x404, Cell3DPosition(-4, -4, 5), 1), Cell3DPosition(-1, -1, 2) }, // Z_EPL1
//     { LRKeyTuple(0x404, Cell3DPosition(-4, -4, 5), 2), Cell3DPosition(0, -1, 1) }, // Z_EPL2
//     { LRKeyTuple(0xA, Cell3DPosition(-4, -4, 5), 3), Cell3DPosition(-1, -1, 2) }, // Z_EPL3
//     { LRKeyTuple(0x840, Cell3DPosition(-4, -4, 5), 4), Cell3DPosition(-1, 0, 1) }, // Z_EPL4
    
// };

// CONTINUOUS FEEDING RULES
static const std::map <const LRKeyTuple, const Cell3DPosition> localMotionRules = 
{
    // Corner XY Cases
    { LRKeyTuple(0x400, Cell3DPosition(-1, -1, 0), 3), Cell3DPosition(0, 1, -1) }, // S_RevZ 3
    { LRKeyTuple(0xC03, Cell3DPosition(-1, -1, 0), 4), Cell3DPosition(-1, 0, 0) }, // S_RevZ 4

    // Border OppX Cases
    { LRKeyTuple(0x380, Cell3DPosition(0, 0, 2), 3), Cell3DPosition(0, -1, 1) }, // Z2 3

    // Border OppY Cases
    { LRKeyTuple(0x940, Cell3DPosition(0, -2, 2), 3), Cell3DPosition(-1, -1, 1) }, // RZ2 3    

    // Pyramid Corner cases
    { LRKeyTuple(0x100, Cell3DPosition(1, 1, 0), 3), Cell3DPosition(1, 0, -1) }, // S_Z 3
    { LRKeyTuple(0x3C0, Cell3DPosition(1, 1, 0), 4), Cell3DPosition(1, 0, 0) }, // S_Z 4    
    
    // Normal Cases
    { LRKeyTuple(0x100, Cell3DPosition(0, 0, 0), 1), Cell3DPosition(0, -1, 0) }, // R 1
    { LRKeyTuple(0x840, Cell3DPosition(0, 0, 0), 2), Cell3DPosition(-1, 0, 1) }, // R 2
    { LRKeyTuple(0x100, Cell3DPosition(0, 0, 0), 3), Cell3DPosition(-1, -1, 0) }, // R 3
    
    { LRKeyTuple(0x840, Cell3DPosition(1, 0, 0), 1), Cell3DPosition(-1, -1, 1) }, // X1 1
    { LRKeyTuple(0x280, Cell3DPosition(0, 1, 0), 1), Cell3DPosition(-1, -1, 1) }, // Y1 1
    { LRKeyTuple(0x220, Cell3DPosition(1, 1, 0), 1), Cell3DPosition(-1, -1, 2) }, // S_Z 1
    { LRKeyTuple(0x808, Cell3DPosition(-1, -1, 0), 1), Cell3DPosition(-1, -1, 2) }, // S_RevZ 1
    { LRKeyTuple(0x200, Cell3DPosition(1, 1, 0), 2), Cell3DPosition(1, 0, 0) }, // S_Z 2
    { LRKeyTuple(0x800, Cell3DPosition(-1, -1, 0), 2), Cell3DPosition(-1, 0, 0) }, // S_RevZ 2
    { LRKeyTuple(0x300, Cell3DPosition(1, 1, 0), 3), Cell3DPosition(1, 0, 0) }, // S_Z 3
    { LRKeyTuple(0xD00, Cell3DPosition(-1, -1, 0), 3), Cell3DPosition(-1, 0, 0) }, // S_RevZ 3
    { LRKeyTuple(0x808, Cell3DPosition(2, 0, 0), 1), Cell3DPosition(0, 0, 1) }, // X2 1
    { LRKeyTuple(0x220, Cell3DPosition(0, 2, 0), 1), Cell3DPosition(0, 0, 1) }, // Y2 1
    { LRKeyTuple(0x100, Cell3DPosition(1, 1, 0), 4), Cell3DPosition(1, 0, -1) }, // S_Z 4
    { LRKeyTuple(0x600, Cell3DPosition(-1, -1, 0), 4), Cell3DPosition(0, 1, -1) }, // S_RevZ 4
    { LRKeyTuple(0x040, Cell3DPosition(2, 0, 0), 2), Cell3DPosition(-1, 0, 1) }, // X2 2
    { LRKeyTuple(0x080, Cell3DPosition(0, 2, 0), 2), Cell3DPosition(0, -1, 1) }, // Y2 2
    { LRKeyTuple(0x900, Cell3DPosition(2, 0, 0), 3), Cell3DPosition(1, 1, -1) }, // X2 3
    { LRKeyTuple(0x300, Cell3DPosition(0, 2, 0), 3), Cell3DPosition(1, 1, -1) }, // Y2 3
    { LRKeyTuple(0x110, Cell3DPosition(0, 0, 1), 1), Cell3DPosition(-1, -1, 2) }, // Z1 1
    { LRKeyTuple(0x404, Cell3DPosition(-1, -1, 1), 1), Cell3DPosition(-1, -1, 2) }, // RevZ1 1
    { LRKeyTuple(0x808, Cell3DPosition(3, 0, 0), 1), Cell3DPosition(0, 0, 1) }, // X3 1
    { LRKeyTuple(0x220, Cell3DPosition(0, 3, 0), 1), Cell3DPosition(0, 0, 1) }, // Y3 1
    { LRKeyTuple(0x100, Cell3DPosition(0, 0, 1), 2), Cell3DPosition(-1, -1, 0) }, // Z1 2
    { LRKeyTuple(0x400, Cell3DPosition(-1, -1, 1), 2), Cell3DPosition(1, 1, 0) }, // RevZ1 2
    { LRKeyTuple(0x042, Cell3DPosition(3, 0, 0), 2), Cell3DPosition(1, 1, 0) }, // X3 2
    { LRKeyTuple(0x081, Cell3DPosition(0, 3, 0), 2), Cell3DPosition(1, 1, 0) }, // Y3 2
    { LRKeyTuple(0x110, Cell3DPosition(0, 0, 2), 1), Cell3DPosition(-1, -1, 2) }, // Z2 1
    { LRKeyTuple(0x404, Cell3DPosition(-2, -2, 2), 1), Cell3DPosition(-1, -1, 2) }, // RevZ2 1
    { LRKeyTuple(0x808, Cell3DPosition(4, 0, 0), 1), Cell3DPosition(0, 0, 1) }, // X4 1
    { LRKeyTuple(0x220, Cell3DPosition(0, 4, 0), 1), Cell3DPosition(0, 0, 1) }, // Y4 1
    { LRKeyTuple(0x100, Cell3DPosition(0, 0, 2), 2), Cell3DPosition(-1, 0, 0) }, // Z2 2
    { LRKeyTuple(0x400, Cell3DPosition(-2, -2, 2), 2), Cell3DPosition(1, 0, 0) }, // RevZ2 2
    { LRKeyTuple(0x042, Cell3DPosition(4, 0, 0), 2), Cell3DPosition(0, 0, 1) }, // X4 2
    { LRKeyTuple(0x081, Cell3DPosition(0, 4, 0), 2), Cell3DPosition(0, 0, 1) }, // Y4 2
    { LRKeyTuple(0x110, Cell3DPosition(0, 0, 3), 1), Cell3DPosition(-1, -1, 2) }, // Z3 1
    { LRKeyTuple(0x404, Cell3DPosition(-3, -3, 3), 1), Cell3DPosition(-1, -1, 2) }, // RevZ3 1
    { LRKeyTuple(0x808, Cell3DPosition(5, 0, 0), 1), Cell3DPosition(0, 0, 1) }, // X5 1
    { LRKeyTuple(0x220, Cell3DPosition(0, 5, 0), 1), Cell3DPosition(0, 0, 1) }, // Y5 1
    { LRKeyTuple(0xB80, Cell3DPosition(0, 0, 2), 3), Cell3DPosition(0, -1, 1) }, // Z2 3
    { LRKeyTuple(0xE02, Cell3DPosition(-2, -2, 2), 3), Cell3DPosition(-1, 0, 1) }, // RevZ2 3
    { LRKeyTuple(0xC00, Cell3DPosition(4, 0, 0), 3), Cell3DPosition(1, 0, 0) }, // X4 3
    { LRKeyTuple(0x600, Cell3DPosition(0, 4, 0), 3), Cell3DPosition(0, 1, 0) }, // Y4 3
    { LRKeyTuple(0x800, Cell3DPosition(4, 0, 0), 4), Cell3DPosition(1, 1, -1) }, // X4 4
    { LRKeyTuple(0x200, Cell3DPosition(0, 4, 0), 4), Cell3DPosition(1, 1, -1) }, // Y4 4
    { LRKeyTuple(0x110, Cell3DPosition(0, 0, 3), 2), Cell3DPosition(-1, -1, 2) }, // Z3 2
    { LRKeyTuple(0x404, Cell3DPosition(-3, -3, 3), 2), Cell3DPosition(-1, -1, 2) }, // RevZ3 2
    { LRKeyTuple(0x042, Cell3DPosition(5, 0, 0), 2), Cell3DPosition(0, 0, 1) }, // X5 2
    { LRKeyTuple(0x081, Cell3DPosition(0, 5, 0), 2), Cell3DPosition(0, 0, 1) }, // Y5 2
    { LRKeyTuple(0x110, Cell3DPosition(0, 0, 4), 1), Cell3DPosition(-1, -1, 2) }, // Z4 1
    { LRKeyTuple(0x404, Cell3DPosition(-4, -4, 4), 1), Cell3DPosition(-1, -1, 2) }, // RevZ4 1
    { LRKeyTuple(0x808, Cell3DPosition(0, -1, 1), 1), Cell3DPosition(-1, -1, 2) }, // RZ1 1
    { LRKeyTuple(0x220, Cell3DPosition(-1, 0, 1), 1), Cell3DPosition(-1, -1, 2) }, // LZ1 1
    { LRKeyTuple(0xC00, Cell3DPosition(5, 0, 0), 3), Cell3DPosition(1, 0, 0) }, // X5 3
    { LRKeyTuple(0x600, Cell3DPosition(0, 5, 0), 3), Cell3DPosition(0, 1, 0) }, // Y5 3
    { LRKeyTuple(0x110, Cell3DPosition(0, 0, 4), 2), Cell3DPosition(-1, 0, 1) }, // Z4 2
    { LRKeyTuple(0x404, Cell3DPosition(-4, -4, 4), 2), Cell3DPosition(0, -1, 1) }, // RevZ4 2
    { LRKeyTuple(0x800, Cell3DPosition(0, -1, 1), 2), Cell3DPosition(-1, 1, 0) }, // RZ1 2
    { LRKeyTuple(0x200, Cell3DPosition(-1, 0, 1), 2), Cell3DPosition(1, -1, 0) }, // LZ1 2
    { LRKeyTuple(0xC00, Cell3DPosition(5, 0, 0), 4), Cell3DPosition(1, 1, 0) }, // X5 4
    { LRKeyTuple(0x600, Cell3DPosition(0, 5, 0), 4), Cell3DPosition(1, 1, 0) }, // Y5 4
    { LRKeyTuple(0xA0, Cell3DPosition(0, 0, 4), 3), Cell3DPosition(-1, -1, 2) }, // Z4 3
    { LRKeyTuple(0xA, Cell3DPosition(-4, -4, 4), 3), Cell3DPosition(-1, -1, 2) }, // RevZ4 3
    { LRKeyTuple(0x100, Cell3DPosition(5, 0, 0), 5), Cell3DPosition(1, 0, -1) }, // X5 5
    { LRKeyTuple(0x100, Cell3DPosition(0, 5, 0), 5), Cell3DPosition(0, 1, -1) }, // Y5 5
    { LRKeyTuple(0x110, Cell3DPosition(0, 0, 5), 1), Cell3DPosition(-1, -1, 2) }, // Z5 1
    { LRKeyTuple(0x404, Cell3DPosition(-5, -5, 5), 1), Cell3DPosition(-1, -1, 2) }, // RevZ5 1
    { LRKeyTuple(0x808, Cell3DPosition(0, -2, 2), 1), Cell3DPosition(-1, -1, 2) }, // RZ2 1
    { LRKeyTuple(0x220, Cell3DPosition(-2, 0, 2), 1), Cell3DPosition(-1, -1, 2) }, // LZ2 1
    { LRKeyTuple(0x200, Cell3DPosition(0, 0, 4), 4), Cell3DPosition(1, 0, 0) }, // Z4 4
    { LRKeyTuple(0x800, Cell3DPosition(-4, -4, 4), 4), Cell3DPosition(-1, 0, 0) }, // RevZ4 4
    { LRKeyTuple(0x110, Cell3DPosition(0, 0, 5), 2), Cell3DPosition(-1, 0, 1) }, // Z5 2
    { LRKeyTuple(0x404, Cell3DPosition(-5, -5, 5), 2), Cell3DPosition(0, -1, 1) }, // RevZ5 2
    { LRKeyTuple(0x800, Cell3DPosition(0, -2, 2), 2), Cell3DPosition(0, 1, 0) }, // RZ2 2
    { LRKeyTuple(0x200, Cell3DPosition(-2, 0, 2), 2), Cell3DPosition(0, -1, 0) }, // LZ2 2
    { LRKeyTuple(0xA0, Cell3DPosition(0, 0, 5), 3), Cell3DPosition(0, 0, 1) }, // Z5 3
    { LRKeyTuple(0xA, Cell3DPosition(-5, -5, 5), 3), Cell3DPosition(-1, -1, 2) }, // RevZ5 3
    { LRKeyTuple(0xD40, Cell3DPosition(0, -2, 2), 3), Cell3DPosition(-1, -1, 1) }, // RZ2 3
    { LRKeyTuple(0x701, Cell3DPosition(-2, 0, 2), 3), Cell3DPosition(0, 0, 1) }, // LZ2 3
    { LRKeyTuple(0x808, Cell3DPosition(0, -3, 3), 1), Cell3DPosition(-1, -1, 2) }, // RZ3 1
    { LRKeyTuple(0x220, Cell3DPosition(-3, 0, 3), 1), Cell3DPosition(-1, -1, 2) }, // LZ3 1
    { LRKeyTuple(0xA0, Cell3DPosition(0, 0, 5), 4), Cell3DPosition(-1, -1, 2) }, // Z5 4
    { LRKeyTuple(0x840, Cell3DPosition(-5, -5, 5), 4), Cell3DPosition(-1, -1, 1) }, // RevZ5 4
    { LRKeyTuple(0x808, Cell3DPosition(0, -3, 3), 2), Cell3DPosition(-1, -1, 2) }, // RZ3 2
    { LRKeyTuple(0x220, Cell3DPosition(-3, 0, 3), 2), Cell3DPosition(-1, -1, 2) }, // LZ3 2
    { LRKeyTuple(0x200, Cell3DPosition(0, 0, 5), 5), Cell3DPosition(1, 0, 0) }, // Z5 5
    { LRKeyTuple(0x800, Cell3DPosition(-5, -5, 5), 5), Cell3DPosition(-1, 0, 0) }, // RevZ5 5
    { LRKeyTuple(0x808, Cell3DPosition(0, -4, 4), 1), Cell3DPosition(-1, -1, 2) }, // RZ4 1
    { LRKeyTuple(0x220, Cell3DPosition(-4, 0, 4), 1), Cell3DPosition(-1, -1, 2) }, // LZ4 1
    { LRKeyTuple(0x808, Cell3DPosition(0, -4, 4), 2), Cell3DPosition(0, 0, 1) }, // RZ4 2
    { LRKeyTuple(0x220, Cell3DPosition(-4, 0, 4), 2), Cell3DPosition(-1, -1, 1) }, // LZ4 2
    { LRKeyTuple(0x050, Cell3DPosition(0, -4, 4), 3), Cell3DPosition(0, -1, 1) }, // RZ4 3
    { LRKeyTuple(0x005, Cell3DPosition(-4, 0, 4), 3), Cell3DPosition(-1, 0, 1) }, // LZ4 3
    { LRKeyTuple(0x808, Cell3DPosition(0, -5, 5), 1), Cell3DPosition(-1, -1, 2) }, // RZ5 1
    { LRKeyTuple(0x220, Cell3DPosition(-5, 0, 5), 1), Cell3DPosition(-1, -1, 2) }, // LZ5 1
    { LRKeyTuple(0x040, Cell3DPosition(0, -4, 4), 4), Cell3DPosition(-1, -1, 1) }, // RZ4 4
    { LRKeyTuple(0x001, Cell3DPosition(-4, 0, 4), 4), Cell3DPosition(0, 0, 1) }, // LZ4 4
    { LRKeyTuple(0x808, Cell3DPosition(0, -5, 5), 2), Cell3DPosition(0, 0, 1) }, // RZ5 2
    { LRKeyTuple(0x220, Cell3DPosition(-5, 0, 5), 2), Cell3DPosition(-1, -1, 1) }, // LZ5 2
    { LRKeyTuple(0x050, Cell3DPosition(0, -5, 5), 3), Cell3DPosition(0, -1, 1) }, // RZ5 3
    { LRKeyTuple(0x005, Cell3DPosition(-5, 0, 5), 3), Cell3DPosition(-1, 0, 1) }, // LZ5 3
    { LRKeyTuple(0x050, Cell3DPosition(0, -5, 5), 4), Cell3DPosition(-1, -1, 2) }, // RZ5 4
    { LRKeyTuple(0x005, Cell3DPosition(-5, 0, 5), 4), Cell3DPosition(-1, -1, 2) }, // LZ5 4
    { LRKeyTuple(0x100, Cell3DPosition(0, -5, 5), 5), Cell3DPosition(0, -1, 0) }, // RZ5 5
    { LRKeyTuple(0x400, Cell3DPosition(-5, 0, 5), 5), Cell3DPosition(0, 1, 0) }, // LZ5 5

    // Vertical Branches Climbing Rules
    { LRKeyTuple(0x404, Cell3DPosition(-4, -5, 5), 1), Cell3DPosition(-1, -1, 2) }, //Z_R_EPL 1
    { LRKeyTuple(0x404, Cell3DPosition(-4, -5, 5), 2), Cell3DPosition(0, -1, 1) }, // Z_R_EPL 2
    { LRKeyTuple(0xA, Cell3DPosition(-4, -5, 5), 3), Cell3DPosition(-1, -1, 2) }, // Z_R_EPL 3
    { LRKeyTuple(0x840, Cell3DPosition(-4, -5, 5), 4), Cell3DPosition(-1, -1, 1) }, //Z_R_EPL 4
    { LRKeyTuple(0x220, Cell3DPosition(-4, 0, 5), 1), Cell3DPosition(-1, -1, 2) }, // RZ_R_EPL1
    { LRKeyTuple(0x220, Cell3DPosition(-4, 0, 5), 2), Cell3DPosition(0, 0, 1) }, // RZ_R_EPL2
    { LRKeyTuple(0x090, Cell3DPosition(-4, 0, 5), 3), Cell3DPosition(-1, 0, 1) }, // RZ_R_EPL3
    { LRKeyTuple(0x090, Cell3DPosition(-4, 0, 5), 4), Cell3DPosition(-1, -1, 2) }, // RZ_R_EPL4
    { LRKeyTuple(0x808, Cell3DPosition(0, -4, 5), 1), Cell3DPosition(-1, -1, 2) }, // LZ_R_EPL1
    { LRKeyTuple(0x808, Cell3DPosition(0, -4, 5), 2), Cell3DPosition(0, 0, 1) }, // LZ_R_EPL2
    { LRKeyTuple(0x050, Cell3DPosition(0, -4, 5), 3), Cell3DPosition(0, -1, 1) }, // LZ_R_EPL3
    { LRKeyTuple(0x050, Cell3DPosition(0, -4, 5), 4), Cell3DPosition(-1, -1, 2) }, // LZ_R_EPL4
    

    { LRKeyTuple(0x404, Cell3DPosition(-5, -4, 5), 1), Cell3DPosition(-1, -1, 2) }, // Z_L_EPL1
    { LRKeyTuple(0x404, Cell3DPosition(-5, -4, 5), 2), Cell3DPosition(-1, 0, 1) }, // Z_L_EPL2
    { LRKeyTuple(0x021, Cell3DPosition(-5, -4, 5), 3), Cell3DPosition(-1, -1, 1) }, // Z_L_EPL3
    { LRKeyTuple(0x021, Cell3DPosition(-5, -4, 5), 4), Cell3DPosition(-1, -1, 2) }, // Z_L_EPL4
    // next 4 rules might be superfluous
    { LRKeyTuple(0x404, Cell3DPosition(-4, -5, 5), 1), Cell3DPosition(-1, -1, 2) }, // Z_R_EPL1
    { LRKeyTuple(0x404, Cell3DPosition(-4, -5, 5), 2), Cell3DPosition(0, -1, 1) }, // Z_R_EPL2
    { LRKeyTuple(0xA, Cell3DPosition(-4, -5, 5), 3), Cell3DPosition(-1, -1, 1) }, // Z_R_EPL3
    { LRKeyTuple(0xA, Cell3DPosition(-4, -5, 5), 4), Cell3DPosition(-1, -1, 2) }, // Z_R_EPL4  

    { LRKeyTuple(0x808, Cell3DPosition(-1, -4, 5), 1), Cell3DPosition(-1, -1, 2) }, // LZ_EPL1
    { LRKeyTuple(0x808, Cell3DPosition(-1, -4, 5), 2), Cell3DPosition(0, 0, 1) }, // LZ_EPL2
    { LRKeyTuple(0x050, Cell3DPosition(-1, -4, 5), 3), Cell3DPosition(-1, -1, 2) }, // LZ_EPL3
    { LRKeyTuple(0x180, Cell3DPosition(-1, -4, 5), 4), Cell3DPosition(-1, -1, 1) }, // LZ_EPL4
    { LRKeyTuple(0x220, Cell3DPosition(-4, -1, 5), 1), Cell3DPosition(-1, -1, 2) }, // RZ_EPL1
    { LRKeyTuple(0x220, Cell3DPosition(-4, -1, 5), 2), Cell3DPosition(-1, -1, 1) }, // RZ_EPL2
    { LRKeyTuple(0x005, Cell3DPosition(-4, -1, 5), 3), Cell3DPosition(-1, -1, 2) }, // RZ_EPL3
    { LRKeyTuple(0x402, Cell3DPosition(-4, -1, 5), 4), Cell3DPosition(0, 0, 1) }, // RZ_EPL4
    { LRKeyTuple(0x110, Cell3DPosition(-1, -1, 5), 1), Cell3DPosition(-1, -1, 2) }, //RevZ_EPL1
    { LRKeyTuple(0x110, Cell3DPosition(-1, -1, 5), 2), Cell3DPosition(-1, 0, 1) }, // RevZ_EPL2
    { LRKeyTuple(0xA0, Cell3DPosition(-1, -1, 5), 3), Cell3DPosition(-1, -1, 2) }, // RevZ_EPL3
    { LRKeyTuple(0x201, Cell3DPosition(-1, -1, 5), 4), Cell3DPosition(0, -1, 1) }, // RevZ_EPL4
    { LRKeyTuple(0x404, Cell3DPosition(-4, -4, 5), 1), Cell3DPosition(-1, -1, 2) }, // Z_EPL1
    { LRKeyTuple(0x404, Cell3DPosition(-4, -4, 5), 2), Cell3DPosition(0, -1, 1) }, // Z_EPL2
    { LRKeyTuple(0xA, Cell3DPosition(-4, -4, 5), 3), Cell3DPosition(-1, -1, 2) }, // Z_EPL3
    { LRKeyTuple(0x840, Cell3DPosition(-4, -4, 5), 4), Cell3DPosition(-1, 0, 1) }, // Z_EPL4
    
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
                                   Cell3DPosition& nextPos) {
    auto match = localMotionRules.find(LRKeyTuple(localNeighborhood,
                                                  tPos - tileRootPos, step));
        
    if (match != localMotionRules.end()) {
        nextPos =  match->second + pos;
        // cout << match->second << endl;
    } else {
        cout << "{ " << localNeighborhood << "("
             << int_to_hex_str((int)localNeighborhood.to_ulong(), 3) << ")"
             << ", " << tPos - tileRootPos << ", " << step << " }" << " -> ";
        cout << "NO MATCH" << endl;
    }        

    return match != localMotionRules.end();
}

#endif /* MESHASSEMBLYLOCALRULES_HPP__ */
    
