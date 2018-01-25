/*
 * pathHop.cpp
 *
 *  Created on: 19/01/2018
 *      Author: pthalamy
 */

#include "pathHop.hpp"

#include <algorithm>

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

