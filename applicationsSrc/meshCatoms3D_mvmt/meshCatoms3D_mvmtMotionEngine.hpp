/**
 * @file   catom3DMotionEngine.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 14:31:33 2018
 * 
 * @brief  
 * 
 * 
 */

#ifndef CATOMS3DMOTIONENGINE_H_
#define CATOMS3DMOTIONENGINE_H_

#include "catoms3DBlock.h"
#include "catoms3DMotionRules.h"
#include "catoms3DWorld.h"

#include "../meltSortGrow/pathHop.hpp"

using namespace Catoms3D;
using namespace BaseSimulator;

class Catoms3DMotionEngine {    
public:
    bool visited = false;
    BlockCode& bc;
    Catoms3DBlock& catom;
    // const ConsoleStream& console;
    
    list<PathHop> path;
    Cell3DPosition goalPosition;
    P2PNetworkInterface *searchParent = NULL;
    Catoms3DMotionRulesLink* nextRotation; //!<
    
    std::map<P2PNetworkInterface*, bool> flag; //!< flags for each neighbors when the node knows that they have been visited
    P2PNetworkInterface *getNextUnprocessedInterface(bool randomly = false);
    
    Catoms3DMotionEngine(BlockCode& _bc, Catoms3DBlock& _catom)
        : bc(_bc), catom(_catom) { }
    
    static inline Catoms3DMotionRules* getMotionRules() {
        return Catoms3DWorld::getWorld()->getMotionRules();
    }

    void resetDFS();
    void attemptMovingTo(const Cell3DPosition& dest);
    bool computeNextRotation(list<PathHop>& path);    
    
    /** 
        @brief Add a new path entry at the back of the path hop list
        @param catom the module for which a hop entry needs to be created
        @param path the path hop list to which the hop entry will be appended
        @param pivotDockingConnector id of the pivot's connector that is connected to the computing catom 
        @param catomDockingConnector id of the computing catom's connector that is connected to the pivot         
        @return false is the catom cannot be connected to the path, true otherwise
    **/
    static bool addModuleToPath(const Catoms3DBlock& catom,
                                list<PathHop>& path,
                                short pivotDockingConnector,
                                short catomDockingConnector);
    
/**
   \brief For a given Catoms3D module acting as a pivot, enumerates all the possible catom surface links that
   a catom connected to the pivot on any connector could use to reach connector conId
   \param pivot The module acting as a pivot for the link search, which will be the point-of-reference for the connector search
   \param conId Identifier of the target connector of the all the link in the results set 
   \param links Reference to the container that will hold the set of all individual surface links around pivot that lead from one connector to connector conId. 
   \return true if at least one link exists, false otherwise
   \remarks This function might not be of any use, as \ref{getAllLinks} might need to be used by default 
   \remarks Changer signature pour renvoyer pathStruct plutot que les links **/
    static bool getAllLinksToConnector(const Catoms3DBlock& pivot,
                                       short conId,
                                       vector<Catoms3DMotionRulesLink*>&links);
    
/**
   \brief For a given Catoms3D module acting as a pivot, enumerates all the possible catom surface links that
   a catom connected to the pivot on any connector could use
   \param pivot The module acting as a pivot for the link search, which will be the point-of-reference for the connector search
   \param links Reference to the container that will hold the set of all individual surface links around pivot that lead from any connector to any other. 
   \return true if at least one link exists, false otherwise
   It can be seen as a graph, where the resulting set is the list of all the edges connecting the connectors of the pivot module, acting as vertices. 
   \remarks This is the fallback function that can be used to find an indirect link from any connector to a target one in case no direct link could be found using \ref{getAllLinksToConnector}  
*/
    static bool getAllLinks(const Catoms3DBlock& pivot,
                            vector<Catoms3DMotionRulesLink*>&links);

    static bool getMotionRulesFromConnector(const Catoms3DBlock& catom,
                                            short conFrom,
                                            vector<Catoms3DMotionRulesLink*>& links);
    
/**
   \brief Given a set of motion rules link passed as argument, searches a path (sequence of individual rotations) that leads from connector conFrom to connector conTo
   \param motionRulesLinks a set of surface links between connectors of a pivot module that another module can follow to rotate
   \param conFrom the source connector of the desired connector path
   \param conTo the destination connector of the desired connector path
   \return an ordered list of individual links that can be followed by a module to move from conFrom to conTo, or list.end() if no path has been found 
   \remarks The best option would be to find the fastest path from conFrom to conTo */
    static Catoms3DMotionRulesLink* findConnectorsPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                                                       short conFrom,
                                                       short conTo,
                                                       Catoms3DBlock& catom);
    
/**
   \brief Given a catom used as pivot, searches a path (sequence of individual rotations) that leads from connector conFrom to connector any connector of the input set
   \param motionRulesLinks a set of surface links between connectors of a pivot module that another module can follow to rotate
   \param conFrom the source connector of the desired connector path
   \param consTo the destinations connector of the desired connector path \attention{sorted in increasing distance to the global path target}
   \param shortestPath  container that will populated as an ordered list of individual links that can be followed by a module to move from conFrom to a connector of conTo, or list.end() if no path has been found 
   \return true if a path exists, false otherwise
   \remarks The best option would be to find the fastest path from conFrom to a conTo connector, and also consider the distance of conFrom to the global target 
   \attention the consTo input vector should be ordered by distance to some target in order for this function to return the shortest path to that target */
    static Catoms3DMotionRulesLink* findConnectorsPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                                                       short conFrom,
                                                       const std::vector<short>& consTo,
                                                       Catoms3DBlock& catom);

    /** 
     * @brief For each connector conFrom in the consFrom set, searches through the graph described by the motionRulesLinks for all the other connectors conOther that are connected to it, and assign it a distance using distance[conOther] = distance[conFrom] + pathLength(conFrom -> conOther)
     * @param motionRulesLinks Motion rules links on which to perform the search
     * @param consFrom Set of input connectors that are desired to be reached
     * @param distance Output map containing for each connector its distance to some target
     * @attention distance must already contain a distance for each of the connectors in the input set consFrom
     * @return true if distance not empty, false otherwise
     */
    static bool computePathConnectorsAndDistances(const Catoms3DBlock& catom,
                                                  std::set<short> consFrom,
                                                  std::map<short, int>& distance);

    /** 
     * @brief Searches through the graph described by the motionRulesLinks for all connectors conOther that are connected to conFrom, and assign it a distance using distance[conOther] = distance[conFrom] + pathLength(conFrom -> conOther)
     * @param motionRulesLinks Motion rules links on which to perform the search
     * @param conFrom Input connectors that is desired to be reached
     * @param distance Output map containing for each connector its distance to some target
     * @attention distance must already contain a distance for the input connectors conFrom
     * @return true if distance not empty, false otherwise
     */
    static bool computePathConnectorsAndDistances(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                                                  const Catoms3DBlock& catom,
                                                  short conFrom,
                                                  std::map<short, int>& distance);

    /** 
     * @brief Initializes a distance map with tailconnector and distance 0
     * @param tailConnector tail connector that will be used for init
     * @param adjacentConnectors Output distance map 
     */
    static void InitializeConnectorsAndDistances(short tailConnector,
                                                 std::map<short, int>& adjacentConnectors);

/**
   @brief Given a set of connector IDs and the orientation of the module to which they belong, determine which connectors of the current module are adjacent to those contained in the input set (Keys of input map). 
   @param catom a pointer to the reference module 
   @param hop the path hop to consider 
   @param pivotDockingConnector id of the pivot's connector that is connected to the computing catom 
   @param catomDockingConnector id of the computing catom's connector that is connected to the pivot 
   @param adjacentConnectors output map of all connectors of module with orientation orientationCode that are adjacent to the connectors in the input set, and their distance to some target */
    static bool findAdjacentConnectorsAndDistances(const Catoms3DBlock& catom,
                                                   const PathHop& hop,
                                                   short pivotDockingConnector,
                                                   short catomDockingConnector,
                                                   std::map<short, int>& adjacentConnectors);

    /**
       @brief Given a set of connector IDs and the orientation of the module to which they belong, determine which connectors of the current module are adjacent to those contained in the input vector
       @param catom a pointer to the reference module 
       @param hop the path hop to consider 
       @param pivotDockingConnector id of the pivot's connector that is connected to the computing catom 
       @param catomDockingConnector id of the computing catom's connector that is connected to the pivot 
       @param adjacentConnectors output vector of all mirrored connectors of module with orientation orientationCode that are adjacent to the connectors in the input set, in the same order as input connectors 
       @param mirrorConnector mapping from mirror adjacent connectors to their input counterparts  */
    static bool findAdjacentConnectors(const Catoms3DBlock& catom,
                                       PathHop& hop,
                                       short pivotDockingConnector,
                                       short catomDockingConnector,
                                       std::vector<short>& adjacentConnectors,
                                       std::map<short, short>& mirrorConnector);

    void trimPath();
    void handleRotationEnd();
    void performNextMoveTowardsGoal();
};

#endif //CATOMS3DMOTIONENGINE_H_
