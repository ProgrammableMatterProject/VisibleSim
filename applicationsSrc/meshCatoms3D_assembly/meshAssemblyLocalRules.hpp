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
    { { std::bitset<12>(0x840), Cell3DPosition(2, 1, -1) }, Cell3DPosition(1, 1, 0) },
    { { std::bitset<12>(0x100), Cell3DPosition(1, 1, 0) }, Cell3DPosition(0, 0, 0) },
    { { std::bitset<12>(0x140), Cell3DPosition(-1, -1, 0) }, Cell3DPosition(2, 0, -1) },
    { { std::bitset<12>(0x180), Cell3DPosition(-1, 1, 0) }, Cell3DPosition(0, 2, -1) },
    { { std::bitset<12>(0x840), Cell3DPosition(1, 0, 0) }, Cell3DPosition(2, 1, -1) },
    { { std::bitset<12>(0x280), Cell3DPosition(0, 1, 0) }, Cell3DPosition(1, 2, -1) },
    { { std::bitset<12>(0x220), Cell3DPosition(1, 1, 0) }, Cell3DPosition(-1, 2, -1) },
    { { std::bitset<12>(0x808), Cell3DPosition(-1, -1, 0) }, Cell3DPosition(2, -1, -1) },
    { { std::bitset<12>(0x200), Cell3DPosition(1, 1, 0) }, Cell3DPosition(-2, 1, 1) },
    { { std::bitset<12>(0x800), Cell3DPosition(-1, -1, 0) }, Cell3DPosition(1, -2, 1) },
    { { std::bitset<12>(0x300), Cell3DPosition(1, 1, 0) }, Cell3DPosition(-1, 1, 1) },
    { { std::bitset<12>(0xC00), Cell3DPosition(-1, -1, 0) }, Cell3DPosition(0, -2, 1) },
    { { std::bitset<12>(0x808), Cell3DPosition(2, 0, 0) }, Cell3DPosition(2, -1, -1) },
    { { std::bitset<12>(0x220), Cell3DPosition(0, 2, 0) }, Cell3DPosition(-1, 2, -1) },
    { { std::bitset<12>(0x400), Cell3DPosition(-1, -1, 0) }, Cell3DPosition(1, -2, 1) },
    { { std::bitset<12>(0x100), Cell3DPosition(1, 1, 0) }, Cell3DPosition(0, 1, 1) },
    { { std::bitset<12>(0x040), Cell3DPosition(2, 0, 0) }, Cell3DPosition(2, -1, -1) },
    { { std::bitset<12>(0x900), Cell3DPosition(2, 0, 0) }, Cell3DPosition(1, -1, 1) },
    { { std::bitset<12>(0x110), Cell3DPosition(0, 0, 1) }, Cell3DPosition(2, 2, -1) },
    { { std::bitset<12>(0x080), Cell3DPosition(0, 2, 0) }, Cell3DPosition(-1, 2, 0) },
    { { std::bitset<12>(0x080), Cell3DPosition(0, 2, 0) }, Cell3DPosition(-1, 2, 0) },
    { { std::bitset<12>(0x300), Cell3DPosition(0, 2, 0) }, Cell3DPosition(-1, 1, 1) },
    { { std::bitset<12>(0x404), Cell3DPosition(-1, -1, 1) }, Cell3DPosition(-1, -1, -1) },
    { { std::bitset<12>(0x808), Cell3DPosition(3, 0, 0) }, Cell3DPosition(2, -1, -1) },
    { { std::bitset<12>(0x220), Cell3DPosition(0, 3, 0) }, Cell3DPosition(-1, 2, -1) },
    { { std::bitset<12>(0x100), Cell3DPosition(0, 0, 1) }, Cell3DPosition(1, 1, 1) },
    { { std::bitset<12>(0x404), Cell3DPosition(-1, -1, 1) }, Cell3DPosition(-2, -2, 1) },
    { { std::bitset<12>(0x808), Cell3DPosition(3, 0, 0) }, Cell3DPosition(2, -1, 0) },
    { { std::bitset<12>(0x220), Cell3DPosition(0, 3, 0) }, Cell3DPosition(-1, 2, 0) },
    { { std::bitset<12>(0x110), Cell3DPosition(1, 1, 1) }, Cell3DPosition(2, 2, -1) },
    { { std::bitset<12>(0x110), Cell3DPosition(1, 1, 1) }, Cell3DPosition(2, 2, -1) },
    { { std::bitset<12>(0x404), Cell3DPosition(-2, -2, 1) }, Cell3DPosition(-1, -1, -1) },
    { { std::bitset<12>(0x808), Cell3DPosition(2, -1, 0) }, Cell3DPosition(2, -1, -1) },
    { { std::bitset<12>(0x220), Cell3DPosition(-1, 2, 0) }, Cell3DPosition(-1, 2, -1) },
    { { std::bitset<12>(0x110), Cell3DPosition(0, 1, 1) }, Cell3DPosition(1, 1, 1) },
    { { std::bitset<12>(0x404), Cell3DPosition(-1, -2, 1) }, Cell3DPosition(-2, -2, 1) },
    { { std::bitset<12>(0x042), Cell3DPosition(2, -1, 1) }, Cell3DPosition(2, -1, 0) },
    { { std::bitset<12>(0x081), Cell3DPosition(-1, 2, 1) }, Cell3DPosition(-1, 2, 0) },
    { { std::bitset<12>(0x110), Cell3DPosition(1, 1, 1) }, Cell3DPosition(2, 2, -1) },
    { { std::bitset<12>(0x404), Cell3DPosition(-2, -2, 1) }, Cell3DPosition(-1, -1, -1) },
    { { std::bitset<12>(0x808), Cell3DPosition(2, -1, 0) }, Cell3DPosition(2, -1, -1) },
    { { std::bitset<12>(0x220), Cell3DPosition(-1, 2, 0) }, Cell3DPosition(-1, 2, -1) },
    { { std::bitset<12>(0xA80), Cell3DPosition(0, 0, 2) }, Cell3DPosition(0, 1, 1) },
    { { std::bitset<12>(0xA02), Cell3DPosition(-2, -2, 2) }, Cell3DPosition(-2, -2, 1) },
    { { std::bitset<12>(0xC00), Cell3DPosition(3, -1, 1) }, Cell3DPosition(2, -1, 0) },
    { { std::bitset<12>(0x600), Cell3DPosition(-1, 3, 1) }, Cell3DPosition(-1, 2, 0) },
    { { std::bitset<12>(0x800), Cell3DPosition(4, 0, 0) }, Cell3DPosition(3, -1, 1) },
    { { std::bitset<12>(0x200), Cell3DPosition(0, 4, 0) }, Cell3DPosition(-1, 3, 1) },
    { { std::bitset<12>(0x110), Cell3DPosition(0, 0, 3) }, Cell3DPosition(1, 1, 1) },
    { { std::bitset<12>(0x404), Cell3DPosition(-3, -3, 3) }, Cell3DPosition(-2, -2, 1) },
    { { std::bitset<12>(0x042), Cell3DPosition(2, -1, 1) }, Cell3DPosition(2, -1, 0) },
    { { std::bitset<12>(0x081), Cell3DPosition(-1, 2, 1) }, Cell3DPosition(-1, 2, 0) },
    { { std::bitset<12>(0x110), Cell3DPosition(1, 1, 1) }, Cell3DPosition(2, 2, -1) },
    { { std::bitset<12>(0x404), Cell3DPosition(-2, -2, 1) }, Cell3DPosition(-1, -1, -1) },
    { { std::bitset<12>(0x808), Cell3DPosition(1, -2, 1) }, Cell3DPosition(2, -1, -1) },
    { { std::bitset<12>(0x220), Cell3DPosition(-2, 1, 1) }, Cell3DPosition(-1, 2, -1) },
    { { std::bitset<12>(0xC00), Cell3DPosition(3, -1, 1) }, Cell3DPosition(2, -1, 1) },
    { { std::bitset<12>(0x600), Cell3DPosition(-1, 3, 1) }, Cell3DPosition(-1, 2, 1) },
    { { std::bitset<12>(0x110), Cell3DPosition(0, 1, 2) }, Cell3DPosition(1, 1, 1) },
    { { std::bitset<12>(0x404), Cell3DPosition(-2, -3, 2) }, Cell3DPosition(-2, -2, 1) },
    { { std::bitset<12>(0x800), Cell3DPosition(0, -1, 1) }, Cell3DPosition(1, -2, 1) },
    { { std::bitset<12>(0x200), Cell3DPosition(-1, 0, 1) }, Cell3DPosition(-2, 1, 1) },
    { { std::bitset<12>(0xC00), Cell3DPosition(4, -1, 1) }, Cell3DPosition(3, -1, 1) },
    { { std::bitset<12>(0x600), Cell3DPosition(-1, 4, 1) }, Cell3DPosition(-1, 3, 1) },
    { { std::bitset<12>(0xA0), Cell3DPosition(-1, 0, 4) }, Cell3DPosition(0, 1, 2) },
    { { std::bitset<12>(0xA), Cell3DPosition(-3, -4, 4) }, Cell3DPosition(-2, -3, 2) },
    { { std::bitset<12>(0x800), Cell3DPosition(5, 0, 0) }, Cell3DPosition(4, -1, 1) },
    { { std::bitset<12>(0x200), Cell3DPosition(0, 5, 0) }, Cell3DPosition(-1, 4, 1) },
    { { std::bitset<12>(0x110), Cell3DPosition(1, 1, 1) }, Cell3DPosition(2, 2, -1) },
    { { std::bitset<12>(0x404), Cell3DPosition(-2, -2, 1) }, Cell3DPosition(-1, -1, -1) },
    { { std::bitset<12>(0x808), Cell3DPosition(1, -2, 1) }, Cell3DPosition(2, -1, -1) },
    { { std::bitset<12>(0x220), Cell3DPosition(-2, 1, 1) }, Cell3DPosition(-1, 2, -1) },
    { { std::bitset<12>(0x200), Cell3DPosition(0, 0, 4) }, Cell3DPosition(-1, 0, 4) },
    { { std::bitset<12>(0x800), Cell3DPosition(-4, -4, 4) }, Cell3DPosition(-3, -4, 4) },
    { { std::bitset<12>(0x110), Cell3DPosition(0, 1, 2) }, Cell3DPosition(1, 1, 1) },
    { { std::bitset<12>(0x404), Cell3DPosition(-2, -3, 2) }, Cell3DPosition(-2, -2, 1) },
    { { std::bitset<12>(0x800), Cell3DPosition(1, -1, 1) }, Cell3DPosition(1, -2, 1) },
    { { std::bitset<12>(0x200), Cell3DPosition(-2, 0, 1) }, Cell3DPosition(-2, 1, 1) },
    { { std::bitset<12>(0xA0), Cell3DPosition(0, 1, 3) }, Cell3DPosition(0, 1, 2) },
    { { std::bitset<12>(0xA), Cell3DPosition(-3, -4, 3) }, Cell3DPosition(-2, -3, 2) },
    { { std::bitset<12>(0xD40), Cell3DPosition(0, -2, 2) }, Cell3DPosition(1, -1, 1) },
    { { std::bitset<12>(0x701), Cell3DPosition(-2, 0, 2) }, Cell3DPosition(-2, 0, 1) },
    { { std::bitset<12>(0x808), Cell3DPosition(1, -2, 1) }, Cell3DPosition(2, -1, -1) },
    { { std::bitset<12>(0x220), Cell3DPosition(-2, 1, 1) }, Cell3DPosition(-1, 2, -1) },
    { { std::bitset<12>(0xA0), Cell3DPosition(-1, 0, 5) }, Cell3DPosition(0, 1, 3) },
    { { std::bitset<12>(0x840), Cell3DPosition(-4, -5, 0) }, Cell3DPosition(-3, -4, 3) },
    { { std::bitset<12>(0x808), Cell3DPosition(0, -3, 3) }, Cell3DPosition(1, -2, 1) },
    { { std::bitset<12>(0x220), Cell3DPosition(-3, 0, 3) }, Cell3DPosition(-2, 1, 1) },
    { { std::bitset<12>(0x200), Cell3DPosition(0, 0, 5) }, Cell3DPosition(-1, 0, 5) },
    { { std::bitset<12>(0x800), Cell3DPosition(-5, -5, 5) }, Cell3DPosition(-4, -5, 0) },
    { { std::bitset<12>(0x808), Cell3DPosition(1, -2, 1) }, Cell3DPosition(2, -1, -1) },
    { { std::bitset<12>(0x220), Cell3DPosition(-2, 1, 1) }, Cell3DPosition(-1, 2, -1) },
    { { std::bitset<12>(0x808), Cell3DPosition(1, -2, 2) }, Cell3DPosition(1, -2, 1) },
    { { std::bitset<12>(0x220), Cell3DPosition(-3, 0, 2) }, Cell3DPosition(-2, 1, 1) },
    { { std::bitset<12>(0x050), Cell3DPosition(1, -3, 3) }, Cell3DPosition(1, -2, 2) },
    { { std::bitset<12>(0x005), Cell3DPosition(-4, 0, 3) }, Cell3DPosition(-3, 0, 2) },
    { { std::bitset<12>(0x808), Cell3DPosition(1, -2, 1) }, Cell3DPosition(2, -1, -1) },
    { { std::bitset<12>(0x220), Cell3DPosition(-2, 1, 1) }, Cell3DPosition(-1, 2, -1) },
    { { std::bitset<12>(0x040), Cell3DPosition(0, -4, 4) }, Cell3DPosition(1, -2, 2) },
    { { std::bitset<12>(0x001), Cell3DPosition(-4, 0, 4) }, Cell3DPosition(-3, 0, 2) },
    { { std::bitset<12>(0x808), Cell3DPosition(1, -2, 1) }, Cell3DPosition(2, -1, -1) },
    { { std::bitset<12>(0x220), Cell3DPosition(-2, 1, 1) }, Cell3DPosition(-1, 2, -1) },
    { { std::bitset<12>(0x808), Cell3DPosition(1, -2, 2) }, Cell3DPosition(1, -2, 1) },
    { { std::bitset<12>(0x220), Cell3DPosition(-3, 0, 2) }, Cell3DPosition(-2, 1, 1) },
    { { std::bitset<12>(0x050), Cell3DPosition(1, -3, 3) }, Cell3DPosition(1, -2, 2) },
    { { std::bitset<12>(0x005), Cell3DPosition(-4, 0, 3) }, Cell3DPosition(-3, 0, 2) },
    { { std::bitset<12>(0x050), Cell3DPosition(0, -4, 5) }, Cell3DPosition(1, -3, 3) },
    { { std::bitset<12>(0x005), Cell3DPosition(-5, -1, 5) }, Cell3DPosition(-4, 0, 3) },
    { { std::bitset<12>(0x100), Cell3DPosition(0, -5, 5) }, Cell3DPosition(0, -4, 5) },
    { { std::bitset<12>(0x400), Cell3DPosition(-5, 0, 5) }, Cell3DPosition(-5, -1, 5) }    
};


/** 
 * Search for the next action among the local rules library 
 * @param localNeighborhood a bitset representing the local neighborhood 
 *  of module at position pos
 * @param pos position of the module awaiting action
 * @return the matched next position if there is one, pos otherwise (meaning no movement)
 */
inline static const Cell3DPosition matchLocalRules(const std::bitset<12>& localNeighborhood,
                                                    const Cell3DPosition& pos,
                                                    const Cell3DPosition& tileRootPos) {
    auto match = localMotionRules.find(make_pair(localNeighborhood, pos));

    return match != localMotionRules.end() ? match->second + tileRootPos : pos + tileRootPos;
}

#endif /* MESHASSEMBLYLOCALRULES_HPP__ */
    
