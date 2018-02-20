/*
 * pathHop.cpp
 *
 *  Created on: 19/01/2018
 *      Author: pthalamy
 */

#include "pathHop.hpp"

#include <algorithm>

#include "utils.h"
#include "catoms3DWorld.h"
#include "catoms3DBlock.h"
#include "lattice.h"

using namespace Catoms3D;

PathHop::PathHop(PathHop const &copy) {
    position = copy.position;
    orientationCode = copy.orientationCode;
    conDistanceMap = std::map<short, int>(copy.conDistanceMap);
}

bool
PathHop::getConnectors(std::set<short>& connectors) const {
    transform(conDistanceMap.begin(), conDistanceMap.end(),
              std::inserter<set<short>>(connectors, connectors.end()),
              [](const std::pair<short,int>& pair) { return pair.first; });

    return !connectors.empty();
}       

bool
PathHop::getConnectorsByIncreasingDistance(vector<short>& sortedCons) {
    // Fill vector with connectors
    for (const auto& pair : conDistanceMap)
        sortedCons.push_back(pair.first);

    // Lambda sort relative to distance to path target
    sort(sortedCons.begin(), sortedCons.end(),
         [=](const short& a, const short& b) {
             return conDistanceMap.at(a) < conDistanceMap.at(b);
         });

    return !sortedCons.empty();
}

bool
PathHop::getMinDistanceConnectors(set<short>& minCons) { 
    vector<short> sortedCons; getConnectorsByIncreasingDistance(sortedCons);

    int distMin = sortedCons.empty() ? -1 : conDistanceMap[sortedCons[0]];
    for (auto it = sortedCons.begin();
         it != sortedCons.end() && conDistanceMap[*it] == distMin;
         it++)
    {
        minCons.insert(*it);
    }
    
    return !minCons.empty();
}

int
PathHop::getDistance(short con) const {
    auto pair = conDistanceMap.find(con);
    if (pair != conDistanceMap.end())
        return pair->second;
    else
        return -1;
}

short
PathHop::getHopConnectorAtPosition(const Cell3DPosition &pos) const {
    Catoms3DBlock *pivot = static_cast<Catoms3DBlock*>(
        Catoms3DWorld::getWorld()->lattice->getBlock(position));

    if (!pivot) {
        cout << "no pivot on position: " << position << endl;
        awaitKeyPressed();
        assert(false);
    }
    
    return pivot->getConnectorId(pos);
}

bool
PathHop::isInVicinityOf(const Cell3DPosition &pos) const {
    short conToFind = getHopConnectorAtPosition(pos);
    
    return conToFind != -1
        && conDistanceMap.find(conToFind) != conDistanceMap.end();  
}

bool
PathHop::catomIsAlreadyOnBestConnector(const Cell3DPosition &pos) {
    set<short> minCons; getMinDistanceConnectors(minCons);
    auto it = minCons.find(getHopConnectorAtPosition(pos));
    bool found = it != minCons.end();
    
    return minCons.empty()
        || found;
}

bool
PathHop::finalTargetReached() const {
    return conDistanceMap.empty();
}

void PathHop::removeConnectorOnPosition(const Cell3DPosition &pos) {
    conDistanceMap.erase(getHopConnectorAtPosition(pos));
}

void
PathHop::prune(short connector) {
    assert(conDistanceMap.find(connector) != conDistanceMap.end());
    int dPrune = conDistanceMap[connector];

    cout << "Prune " << connector << "(" << dPrune << ")" << endl;
    
    utils::erase_if(conDistanceMap, [&](std::pair<short, int> pair) {
            return (pair.second >= dPrune); // (pair.first != connector) && ?
        });

    cout << "Res: " << *this << endl;    
}

std::ostream& operator<<(std::ostream &stream, PathHop const& hop) {   
    stream << "Position: " << hop.position << endl
           << "Orientation: " << hop.orientationCode << endl
           << "Connectors: " << endl;

    for (auto const& pair : hop.conDistanceMap) {
        stream << pair.first << " (d = " << pair.second << ")" << endl;
    }
    
    return stream;
}
