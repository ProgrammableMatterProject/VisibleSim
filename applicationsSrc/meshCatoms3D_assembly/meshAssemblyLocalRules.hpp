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
    
template<size_t sz> struct localrule_comparer {
    bool operator() (const std::pair<const std::bitset<sz>, const Cell3DPosition> &p1,
                     const std::pair<const std::bitset<sz>, const Cell3DPosition> &p2) const {
        if (p1.first.to_ulong() < p2.first.to_ulong()) return true;
        else if (p1.first.to_ulong() > p2.first.to_ulong()) return false;
        else return p1.second < p2.second;
    }
};

static const std::map <const std::pair<const std::bitset<12>, const Cell3DPosition>,
                       const Cell3DPosition, localrule_comparer<12>>
localMotionRules = 
{
    // Corner XY Cases
    { { bitset<12>(0x400), Cell3DPosition(-1, -1, 0) }, Cell3DPosition(0, -1, 0) }, // S_RevZ
    { { bitset<12>(0xC03), Cell3DPosition(-1, -1, 0) }, Cell3DPosition(-1, -1, 0) }, // S_RevZ

    // Normal Cases
    { { bitset<12>(0x840), Cell3DPosition(0, 0, 0) }, Cell3DPosition(1, 1, 0) }, // R
    { { bitset<12>(0x100), Cell3DPosition(0, 0, 0) }, Cell3DPosition(0, 0, 0) }, // R
    { { bitset<12>(0x140), Cell3DPosition(1, -1, 0) }, Cell3DPosition(1, -1, 0) }, // S_RZ
    { { bitset<12>(0x180), Cell3DPosition(-1, 1, 0) }, Cell3DPosition(-1, 1, 0) }, // S_LZ
    { { bitset<12>(0x840), Cell3DPosition(1, 0, 0) }, Cell3DPosition(1, 0, 0) }, // X1
    { { bitset<12>(0x280), Cell3DPosition(0, 1, 0) }, Cell3DPosition(0, 1, 0) }, // Y1
    { { bitset<12>(0x220), Cell3DPosition(1, 1, 0) }, Cell3DPosition(-2, 1, 1) }, // S_Z
    { { bitset<12>(0x808), Cell3DPosition(-1, -1, 0) }, Cell3DPosition(1, -2, 1) }, // S_RevZ
    { { bitset<12>(0x200), Cell3DPosition(1, 1, 0) }, Cell3DPosition(-1, 1, 1) }, // S_Z
    { { bitset<12>(0x800), Cell3DPosition(-1, -1, 0) }, Cell3DPosition(0, -2, 1) }, // S_RevZ
    { { bitset<12>(0x300), Cell3DPosition(1, 1, 0) }, Cell3DPosition(0, 1, 1) }, // S_Z
    { { bitset<12>(0xC00), Cell3DPosition(-1, -1, 0) }, Cell3DPosition(1, -2, 1) }, // S_RevZ
    { { bitset<12>(0x808), Cell3DPosition(2, 0, 0) }, Cell3DPosition(2, -1, 0) }, // X2
    { { bitset<12>(0x220), Cell3DPosition(0, 2, 0) }, Cell3DPosition(-1, 2, 0) }, // Y2
    { { bitset<12>(0x100), Cell3DPosition(1, 1, 0) }, Cell3DPosition(1, 1, 0) }, // S_Z
    { { bitset<12>(0x400), Cell3DPosition(-1, -1, 0) }, Cell3DPosition(-1, -1, 0) }, // S_RevZ
    { { bitset<12>(0x040), Cell3DPosition(2, 0, 0) }, Cell3DPosition(1, -1, 1) }, // X2
    { { bitset<12>(0x080), Cell3DPosition(0, 2, 0) }, Cell3DPosition(-1, 1, 1) }, // Y2
    { { bitset<12>(0x900), Cell3DPosition(2, 0, 0) }, Cell3DPosition(2, 0, 0) }, // X2
    { { bitset<12>(0x300), Cell3DPosition(0, 2, 0) }, Cell3DPosition(0, 2, 0) }, // Y2
    { { bitset<12>(0x110), Cell3DPosition(0, 0, 1) }, Cell3DPosition(1, 1, 1) }, // Z1
    { { bitset<12>(0x404), Cell3DPosition(-1, -1, 1) }, Cell3DPosition(-2, -2, 1) }, // RevZ1
    { { bitset<12>(0x808), Cell3DPosition(3, 0, 0) }, Cell3DPosition(2, -1, 0) }, // X3
    { { bitset<12>(0x220), Cell3DPosition(0, 3, 0) }, Cell3DPosition(-1, 2, 0) }, // Y3
    { { bitset<12>(0x100), Cell3DPosition(0, 0, 1) }, Cell3DPosition(0, 0, 1) }, // Z1
    { { bitset<12>(0x404), Cell3DPosition(-1, -1, 1) }, Cell3DPosition(-1, -1, 1) }, // RevZ1
    { { bitset<12>(0x042), Cell3DPosition(3, 0, 0) }, Cell3DPosition(3, 0, 0) }, // X3
    { { bitset<12>(0x081), Cell3DPosition(0, 3, 0) }, Cell3DPosition(0, 3, 0) }, // Y3
    { { bitset<12>(0x110), Cell3DPosition(0, 0, 2) }, Cell3DPosition(1, 1, 1) }, // Z2
    { { bitset<12>(0x404), Cell3DPosition(-2, -2, 2) }, Cell3DPosition(-2, -2, 1) }, // RevZ2
    { { bitset<12>(0x808), Cell3DPosition(4, 0, 0) }, Cell3DPosition(2, -1, 0) }, // X4
    { { bitset<12>(0x220), Cell3DPosition(0, 4, 0) }, Cell3DPosition(-1, 2, 0) }, // Y4
    { { bitset<12>(0x110), Cell3DPosition(0, 0, 2) }, Cell3DPosition(0, 1, 1) }, // Z2
    { { bitset<12>(0x404), Cell3DPosition(-2, -2, 2) }, Cell3DPosition(-1, -2, 1) }, // RevZ2
    { { bitset<12>(0x042), Cell3DPosition(4, 0, 0) }, Cell3DPosition(2, -1, 1) }, // X4
    { { bitset<12>(0x081), Cell3DPosition(0, 4, 0) }, Cell3DPosition(-1, 2, 1) }, // Y4
    { { bitset<12>(0x110), Cell3DPosition(0, 0, 3) }, Cell3DPosition(1, 1, 1) }, // Z3
    { { bitset<12>(0x404), Cell3DPosition(-3, -3, 3) }, Cell3DPosition(-2, -2, 1) }, // RevZ3
    { { bitset<12>(0x808), Cell3DPosition(5, 0, 0) }, Cell3DPosition(2, -1, 0) }, // X5
    { { bitset<12>(0x220), Cell3DPosition(0, 5, 0) }, Cell3DPosition(-1, 2, 0) }, // Y5
    { { bitset<12>(0xA80), Cell3DPosition(0, 0, 2) }, Cell3DPosition(0, 0, 2) }, // Z2
    { { bitset<12>(0xA02), Cell3DPosition(-2, -2, 2) }, Cell3DPosition(-2, -2, 2) }, // RevZ2
    { { bitset<12>(0xC00), Cell3DPosition(4, 0, 0) }, Cell3DPosition(3, -1, 1) }, // X4
    { { bitset<12>(0x600), Cell3DPosition(0, 4, 0) }, Cell3DPosition(-1, 3, 1) }, // Y4
    { { bitset<12>(0x800), Cell3DPosition(4, 0, 0) }, Cell3DPosition(4, 0, 0) }, // X4
    { { bitset<12>(0x200), Cell3DPosition(0, 4, 0) }, Cell3DPosition(0, 4, 0) }, // Y4
    { { bitset<12>(0x110), Cell3DPosition(0, 0, 3) }, Cell3DPosition(0, 0, 3) }, // Z3
    { { bitset<12>(0x404), Cell3DPosition(-3, -3, 3) }, Cell3DPosition(-3, -3, 3) }, // RevZ3
    { { bitset<12>(0x042), Cell3DPosition(5, 0, 0) }, Cell3DPosition(2, -1, 1) }, // X5
    { { bitset<12>(0x081), Cell3DPosition(0, 5, 0) }, Cell3DPosition(-1, 2, 1) }, // Y5
    { { bitset<12>(0x110), Cell3DPosition(0, 0, 4) }, Cell3DPosition(1, 1, 1) }, // Z4
    { { bitset<12>(0x404), Cell3DPosition(-4, -4, 4) }, Cell3DPosition(-2, -2, 1) }, // RevZ4
    { { bitset<12>(0x808), Cell3DPosition(0, -1, 1) }, Cell3DPosition(1, -2, 1) }, // RZ1
    { { bitset<12>(0x220), Cell3DPosition(-1, 0, 1) }, Cell3DPosition(-2, 1, 1) }, // LZ1
    { { bitset<12>(0xC00), Cell3DPosition(5, 0, 0) }, Cell3DPosition(3, -1, 1) }, // X5
    { { bitset<12>(0x600), Cell3DPosition(0, 5, 0) }, Cell3DPosition(-1, 3, 1) }, // Y5
    { { bitset<12>(0x110), Cell3DPosition(0, 0, 4) }, Cell3DPosition(0, 1, 2) }, // Z4
    { { bitset<12>(0x404), Cell3DPosition(-4, -4, 4) }, Cell3DPosition(-2, -3, 2) }, // RevZ4
    { { bitset<12>(0x800), Cell3DPosition(0, -1, 1) }, Cell3DPosition(0, -1, 1) }, // RZ1
    { { bitset<12>(0x200), Cell3DPosition(-1, 0, 1) }, Cell3DPosition(-1, 0, 1) }, // LZ1
    { { bitset<12>(0xC00), Cell3DPosition(5, 0, 0) }, Cell3DPosition(4, -1, 1) }, // X5
    { { bitset<12>(0x600), Cell3DPosition(0, 5, 0) }, Cell3DPosition(-1, 4, 1) }, // Y5
    { { bitset<12>(0xA0), Cell3DPosition(0, 0, 4) }, Cell3DPosition(-1, 0, 4) }, // Z4
    { { bitset<12>(0xA), Cell3DPosition(-4, -4, 4) }, Cell3DPosition(-3, -4, 4) }, // RevZ4
    { { bitset<12>(0x800), Cell3DPosition(5, 0, 0) }, Cell3DPosition(5, 0, 0) }, // X5
    { { bitset<12>(0x200), Cell3DPosition(0, 5, 0) }, Cell3DPosition(0, 5, 0) }, // Y5
    { { bitset<12>(0x110), Cell3DPosition(0, 0, 5) }, Cell3DPosition(1, 1, 1) }, // Z5
    { { bitset<12>(0x404), Cell3DPosition(-5, -5, 5) }, Cell3DPosition(-2, -2, 1) }, // RevZ5
    { { bitset<12>(0x808), Cell3DPosition(0, -2, 2) }, Cell3DPosition(1, -2, 1) }, // RZ2
    { { bitset<12>(0x220), Cell3DPosition(-2, 0, 2) }, Cell3DPosition(-2, 1, 1) }, // LZ2
    { { bitset<12>(0x200), Cell3DPosition(0, 0, 4) }, Cell3DPosition(0, 0, 4) }, // Z4
    { { bitset<12>(0x800), Cell3DPosition(-4, -4, 4) }, Cell3DPosition(-4, -4, 4) }, // RevZ4
    { { bitset<12>(0x110), Cell3DPosition(0, 0, 5) }, Cell3DPosition(0, 1, 2) }, // Z5
    { { bitset<12>(0x404), Cell3DPosition(-5, -5, 5) }, Cell3DPosition(-2, -3, 2) }, // RevZ5
    { { bitset<12>(0x800), Cell3DPosition(0, -2, 2) }, Cell3DPosition(1, -1, 1) }, // RZ2
    { { bitset<12>(0x200), Cell3DPosition(-2, 0, 2) }, Cell3DPosition(-2, 0, 1) }, // LZ2
    { { bitset<12>(0xA0), Cell3DPosition(0, 0, 5) }, Cell3DPosition(0, 1, 3) }, // Z5
    { { bitset<12>(0xA), Cell3DPosition(-5, -5, 5) }, Cell3DPosition(-3, -4, 3) }, // RevZ5
    { { bitset<12>(0xD40), Cell3DPosition(0, -2, 2) }, Cell3DPosition(0, -2, 2) }, // RZ2
    { { bitset<12>(0x701), Cell3DPosition(-2, 0, 2) }, Cell3DPosition(-2, 0, 2) }, // LZ2
    { { bitset<12>(0x808), Cell3DPosition(0, -3, 3) }, Cell3DPosition(1, -2, 1) }, // RZ3
    { { bitset<12>(0x220), Cell3DPosition(-3, 0, 3) }, Cell3DPosition(-2, 1, 1) }, // LZ3
    { { bitset<12>(0xA0), Cell3DPosition(0, 0, 5) }, Cell3DPosition(-1, 0, 5) }, // Z5
    { { bitset<12>(0x840), Cell3DPosition(-5, -5, 5) }, Cell3DPosition(-4, -5, 0) }, // RevZ5
    { { bitset<12>(0x808), Cell3DPosition(0, -3, 3) }, Cell3DPosition(0, -3, 3) }, // RZ3
    { { bitset<12>(0x220), Cell3DPosition(-3, 0, 3) }, Cell3DPosition(-3, 0, 3) }, // LZ3
    { { bitset<12>(0x200), Cell3DPosition(0, 0, 5) }, Cell3DPosition(0, 0, 5) }, // Z5
    { { bitset<12>(0x800), Cell3DPosition(-5, -5, 5) }, Cell3DPosition(-5, -5, 5) }, // RevZ5
    { { bitset<12>(0x808), Cell3DPosition(0, -4, 4) }, Cell3DPosition(1, -2, 1) }, // RZ4
    { { bitset<12>(0x220), Cell3DPosition(-4, 0, 4) }, Cell3DPosition(-2, 1, 1) }, // LZ4
    { { bitset<12>(0x808), Cell3DPosition(0, -4, 4) }, Cell3DPosition(1, -2, 2) }, // RZ4
    { { bitset<12>(0x220), Cell3DPosition(-4, 0, 4) }, Cell3DPosition(-3, 0, 2) }, // LZ4
    { { bitset<12>(0x050), Cell3DPosition(0, -4, 4) }, Cell3DPosition(1, -3, 3) }, // RZ4
    { { bitset<12>(0x005), Cell3DPosition(-4, 0, 4) }, Cell3DPosition(-4, 0, 3) }, // LZ4
    { { bitset<12>(0x808), Cell3DPosition(0, -5, 5) }, Cell3DPosition(1, -2, 1) }, // RZ5
    { { bitset<12>(0x220), Cell3DPosition(-5, 0, 5) }, Cell3DPosition(-2, 1, 1) }, // LZ5
    { { bitset<12>(0x040), Cell3DPosition(0, -4, 4) }, Cell3DPosition(0, -4, 4) }, // RZ4
    { { bitset<12>(0x001), Cell3DPosition(-4, 0, 4) }, Cell3DPosition(-4, 0, 4) }, // LZ4
    { { bitset<12>(0x808), Cell3DPosition(0, -5, 5) }, Cell3DPosition(1, -2, 1) }, // RZ5
    { { bitset<12>(0x220), Cell3DPosition(-5, 0, 5) }, Cell3DPosition(-2, 1, 1) }, // LZ5
    { { bitset<12>(0x808), Cell3DPosition(0, -5, 5) }, Cell3DPosition(1, -2, 2) }, // RZ5
    { { bitset<12>(0x220), Cell3DPosition(-5, 0, 5) }, Cell3DPosition(-3, 0, 2) }, // LZ5
    { { bitset<12>(0x050), Cell3DPosition(0, -5, 5) }, Cell3DPosition(1, -3, 3) }, // RZ5
    { { bitset<12>(0x005), Cell3DPosition(-5, 0, 5) }, Cell3DPosition(-4, 0, 3) }, // LZ5
    { { bitset<12>(0x050), Cell3DPosition(0, -5, 5) }, Cell3DPosition(0, -4, 5) }, // RZ5
    { { bitset<12>(0x005), Cell3DPosition(-5, 0, 5) }, Cell3DPosition(-5, -1, 5) }, // LZ5
    { { bitset<12>(0x100), Cell3DPosition(0, -5, 5) }, Cell3DPosition(0, -5, 5) }, // RZ5
    { { bitset<12>(0x400), Cell3DPosition(-5, 0, 5) }, Cell3DPosition(-5, 0, 5) } // LZ5 
};


/** 
 * Search for the next action among the local rules library 
 * @param localNeighborhood a bitset representing the local neighborhood 
 *  of module at position pos
 * @param tPos final target position of the module awaiting action
 * @param tileRootPos the position of the nearest tile root, for normalizing the coordinates
 * @param nextPos next position that the moving module has occupy
 * @return the matched next position if there is one, pos otherwise (meaning no movement)
 */
inline static bool matchLocalRules(const std::bitset<12>& localNeighborhood,
                                   const Cell3DPosition& tPos,
                                   const Cell3DPosition& tileRootPos,
                                   Cell3DPosition& nextPos) {
    cout << "{ " << localNeighborhood << "("
         << int_to_hex_str((int)localNeighborhood.to_ulong(), 3) << ")"
         << ", " << tPos - tileRootPos << " }" << " -> ";

    auto match = localMotionRules.find(make_pair(localNeighborhood, tPos - tileRootPos));
        
    if (match != localMotionRules.end()) {
        nextPos =  match->second + tileRootPos;
        cout << match->second << endl;
    } else {
        cout << "NO MATCH" << endl;
    }        

    return match != localMotionRules.end();
}

#endif /* MESHASSEMBLYLOCALRULES_HPP__ */
    
