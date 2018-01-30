/*
 * pathHop.hpp
 *
 *  Created on: 19/01/2018
 *      Author: pthalamy
 */

#ifndef PATH_HOP_H_
#define PATH_HOP_H_

#include <map>
#include <vector>
#include <set>

#include "cell3DPosition.h"

/** @brief A hop in a path made of Catoms3D connectors **/
class PathHop {
    Cell3DPosition position; /// Lattice position of the hop module
    short orientationCode; /// Determines the orientation of the hop module ori \in [0..23], where c \in [0..11] (the connector on the x axis), a \in [1..2] (the orientation of the connector, 1 = regular or 2 = upside-down) and ori = c * a
    std::map<short, int> conDistanceMap; /// A dictionary where each key is the ID of a connector in the path and each value its distance in number of rotations to the movement target using the shortest path

public:
    PathHop(Cell3DPosition pos, short ori, std::map<short, int> map)
        : position(pos), orientationCode(ori), conDistanceMap(map) {}
    ~PathHop(){}

    inline short getOrientation() const { return orientationCode; }
    inline Cell3DPosition getPosition() const { return position; }
    bool getConnectors(std::set<short>& connectors) const;
    /** 
     * @brief Returns the distance corresponding to connector con
     * @param con input connector
     * @return the distance of the connector to some target, or -1 if connector is not in path hop
     */
    int getDistance(short con) const;
    
    /**
       \brief Populates a vector with all the connectors in the \see{conDistanceMap}, sorted by increasing distance to the path target
       \param sortedCons reference to output solution vector
       \return true if vector is not empty, false otherwise **/
    bool getConnectorsByIncreasingDistance(vector<short>& sortedCons);

    /** 
     * @brief Indicates whether cell in argument is in a position of the current path hop
     * @param pos cell that may or may not belong to hop
     * @return true if pos if is a position of this hop, false otherwise
     */
    bool isInVicinityOf(const Cell3DPosition &pos) const;

    /** 
     * @brief Called once a connector has been selected, removes connectors with higher distance than connector in argument from the hop
     * @param connector connector that has been choosen for movement */
    void prune(short connector);

    bool operator==(const PathHop &o) const
        { return o.position == position; };
    bool operator!=(const PathHop &o) const
        { return o.position != position; };

    friend std::ostream& operator<<(std::ostream &stream, PathHop const& hop);
};

std::ostream& operator<<(std::ostream &stream, PathHop const& hop);

#endif /* PATH_HOP_H_ */
