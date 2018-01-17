
#ifndef __DESIGN_HPP_
#define __DESIGN_HPP_

#include <set>
#include <list>

#include "catoms3DMotionRules.h"

using namespace Catoms3D;

/**
   @brief blablabla
   @param x 
   @return */
// void function(...);

class API { 

public:
/**
   @brief For a given Catoms3D module acting as a pivot, enumerates all the possible catom surface links that
   a catom connected to the pivot on any connector could use to reach connector conId
   @param pivot The module acting as a pivot for the link search, which will be the point-of-reference for the connector search
   @param conId Identifier of the target connector of the all the link in the results set 
   @return A set of all possible individual surface links around pivot that lead from one connector to connector conId. 
   @remarks This function might not be of any use, as @ref{getAllLinks} might need to be used by default */
    static vector<Catoms3DMotionRulesLink*> getAllLinksToConnector(const Catoms3DBlock *pivot,
                                                                short conId);
    
/**
   @brief For a given Catoms3D module acting as a pivot, enumerates all the possible catom surface links that
   a catom connected to the pivot on any connector could use
   @param pivot The module acting as a pivot for the link search, which will be the point-of-reference for the connector search
   @return A set of all possible individual surface links around pivot that lead from one connector to another. 
   It can be seen as a graph, where the resulting set is the list of all the edges connecting the connectors of the pivot module, acting as vertices. 
   @remarks This is the fallback function that can be used to find an indirect link from any connector to a target one in case no direct link could be found using \ref{getAllLinksToConnector}  */
    static vector<Catoms3DMotionRulesLink*> getAllLinks(const Catoms3DBlock *pivot);

/**
   @brief Given a set of motion rules link passed as argument, searches a path (sequence of individual rotations) that leads from connector conFrom to connector conTo
   @param motionRulesLinks a set of surface links between connectors of a pivot module that another module can follow to rotate
   @param conFrom the source connector of the desired connector path
   @param conTo the destination connector of the desired connector path
   @return an ordered list of individual links that can be followed by a module to move from conFrom to conTo, or list.end() if no path has been found 
   @remarks The best option would be to find the fastest path from conFrom to conTo*/
    static list<Catoms3DMotionRulesLink*> findConnectorsPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                                                     short conFrom,
                                                     short conTo);


/**
   @brief Given a catom used as pivot, searches a path (sequence of individual rotations) that leads from connector conFrom to connector any connector of the input set
   @param motionRulesLinks a set of surface links between connectors of a pivot module that another module can follow to rotate
   @param conFrom the source connector of the desired connector path
   @param consTo the destinations connector of the desired connector path
   @return an ordered list of individual links that can be followed by a module to move from conFrom to a connector of conTo, or list.end() if no path has been found 
   @remarks The best option would be to find the fastest path from conFrom to a conTo connector */
    static list<Catoms3DMotionRulesLink*> findConnectorsPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                                                     short conFrom,
                                                     set<short> consTo);

/**
   @brief Given a set of motion rules link passed as argument, deduce a set of all connectors for which a path exists to connector conTo
   @param motionRulesLinks a set of surface links between connectors of a pivot module that another module can follow to rotate
   @param conTo the desired destination of the connector path for which we want to find all potential motion sources
   @return a set of all connectors for which a path to conTo exists 
   @remarks An enhancement could be to return a list of connectors sorted by increasing distance to conTo */
    static set<short> findPathConnectors(const set<Catoms3DMotionRulesLink*>& motionRulesLinks,
                                         short conTo);

/**
   @brief Given a catom used as pivot, deduce a set of all connectors for which a path exists to connector conTo
   @param conTo the desired destination of the connector path for which we want to find all potential motion sources
   @return a set of all connectors for which a path to conTo exists 
   @remarks An enhancement could be to return a list of connectors sorted by increasing distance to conTo */
    static set<short> findPathConnectors(const Catoms3DBlock *pivot, short conTo);

/**
   @brief Given a catom used as pivot, deduce a set of all connectors for which a path exists to any connector of input set consTo
   @param consTo the desired destinations of the connector path for which we want to find all potential motion sources
   @return a set of all connectors for which a path to conTo exists 
   @remarks An enhancement could be to return a list of connectors sorted by increasing distance to conTo */
    static set<short> findPathConnectors(const Catoms3DBlock *pivot, set<short> consTo);

/**
   @brief Given a set of connector IDs and the orientation of the module to which they belong, determine which connectors of the current module are adjacent to those contained in the input set. 
   @remarks Two connectors of opposing modules are adjacent if a third module could fill a position where both connectors are connected to that module at the same time
   @param pathConnectors a set of connector IDs belonging to a path to some target
   @param pathOrientationCode the orientation code of the module to which the path connectors belong
   @param orientationCode the orientation code of the module for which we wish to determine the connectors adjacent to the input set
   @return a set of all connectors of module with orientation orientationCode that are adjacent to the connectors in the input set, set.end() if none exist */
    static set<short> findAdjacentConnectors(const set<short>& pathConnectors,
                                      short pathOrientationCode,
                                      short orientationCode);


// NOT MSG SPECIFIC

/**
   @brief Returns the ID of the connector of a given catom that is adjacent to the input lattice position
   @remarks Two connectors of opposing modules are adjacent if a third module could fill a position where both connectors are connected to that module at the same time
   @param catom the module for which we want to find which connector is adjacent to the input position
   @param cell a cell position that belongs to the simulated lattice
   @return the ID of the connector of module catom that is adjacent to the input position, or -1 if that position is not in the neighborhood of catom */
    static set<short> getConnectorsAdjacentToCell(Catoms3DBlock *catom,
                                           const Cell3DPosition cell);

/**
   @brief Returns the ID of the connector of a given catom that is connected to the input lattice position
   @param catom the module for which we want to find which connector is adjacent to the input position
   @param cell a cell cell that belongs to the simulated lattice
   @return the ID of the connector of module catom that is adjacent to the input cell, or -1 if that cell is not in the neighborhood of catom */
    static short getConnectorForCell(Catoms3DBlock *catom, const Cell3DPosition cell);
// Already implemented in Catoms3DBlock::getConnectorId(cell);
};

#endif
