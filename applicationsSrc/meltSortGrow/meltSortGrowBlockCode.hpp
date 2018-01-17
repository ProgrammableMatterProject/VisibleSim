/*
 * meltSortGrowBlockCode.h
 *
 *  Created on: 07/11/17
 *      Author: pthalamy
 */

#ifndef MELTSORTGROWBLOCKCODE_H_
#define MELTSORTGROWBLOCKCODE_H_

#include <list>
#include <set>
#include <unordered_set>
#include <map>
#include <climits>

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DMotionRules.h"

#include "rotation3DEvents.h"
#include "catoms3DBlock.h"

#include "msgMessages.hpp"

class MeltSortGrowBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
    Scheduler *scheduler;
    Catoms3D::Catoms3DBlock *catom;
    MeltSortGrowBlockCode(Catoms3D::Catoms3DBlock *host);
    ~MeltSortGrowBlockCode();          

    /**
     * @brief Global message handler for this instance of the blockcode
     * @param msg Message received by the module
     * @param sender Connector that has received the message and hence that is connected to the sender */
    void processReceivedMessage(MessagePtr msg, P2PNetworkInterface* sender);
    
    void startup();
    void processLocalEvent(EventPtr pev);

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new MeltSortGrowBlockCode((Catoms3DBlock*)host));
    }

/* private: */
    RelativeTargetGrid *rtg; //!< The target reconfiguration expressed as a RelativeTargetGrid
    Cell3DPosition *relPos = NULL; //!< The position of the module, relative to the root
     
    /* Root Election Variables */
    Cell3DPosition currentRootPosition;
    unsigned int expectedConfirms;
    P2PNetworkInterface *parent;     //!< Connector connected to the module to confirm to

    /**** MELT *****/

    /* Articulation Points (AP) Labelling */
    /* See: Ahuja, Mohan, and Yahui Zhu. “An Efficient Distributed Algorithm for Finding Articulation Points, Bridges, and Biconnected Components in Asynchronous Networks.” In Foundations of Software Technology and Theoretical Computer Science, 99–108. Springer, 1989. */
    int dfn = 0; //!< Depth-First number (order in which a DFS first visited this node)
    int lDfn = 0; //!< Lowest DFN reachable from tis node using a path of descendants followed by at most one back edge
    int dfnCnt = 0; //!< DFN counter at this node
    int minDfn = INT_MAX; //!< Minimum FDN received by node from its ancestors other than its father
    P2PNetworkInterface *minSdr = NULL; //!< Id of the node which sent the minDfn
    enum APLState { Inactive, Active }; //!< Possible node states in AP labelling algorithms
    APLState state = APLState::Inactive;
    P2PNetworkInterface *father = NULL; //!< Connector connected to the node's father
    std::unordered_set<P2PNetworkInterface*> sons; //!< The node's children
    bool source = false; //!< Indicates whether module is source of the DFS
    bool bridge = false; //!< Indicates whether edge with father is a bridge
    bool articulationPoint = false; //!< Indicates whether node is an AP
    std::vector<P2PNetworkInterface*> neighbors; //!< Active neighbors around module
    std::map<P2PNetworkInterface*, bool> flag; //!< flags for each neighbors when the node knows that they have been visited
    int resetChildrenDecount; //!< Number of children which have NOT YET notified their father that they reset their search data structures
    P2PNetworkInterface *resetFather = NULL; //!< interface connected to the module which sent us a reset command first
    bool melted = false; //!< indicates whether module has already melted into the line
    Cell3DPosition *tailPosition = NULL; //!< if defined, next position to be filled by the melt algorithm    

    /* Newest Path Discovery */
    list<Cell3DPosition> pathPositions; //!< Current Melt path being discovered
    set<Cell3DPosition> processedPP; //!< Set containing all the path positions that have been processed without finding a mobile module so far
    P2PNetworkInterface *pathPosPred; //!< Predecessor module in the path position discovery (might not be useful)
    vector<Catoms3DMotionRulesLink*> pathLinks; //!< All possible connector links which consists in the path to a mobile module
    short tailConId; //!< Connector id corresponding to the position to be filled by the tail
    Catoms3DMotionRules motionRules; //!< Motion rules engine for local Catoms3D motion planning
    set<short> pathConnectors; /// 
    set<short> parentPathConnectors; ///
    P2PNetworkInterface *meltFather;
    
    /**
     * @brief Searches among potential path positions around module and returns the next candidate cell
     * @param lastPP the last path position to consider, from which we are trying to find a path
     * @param nextPP (out) if found, the new candidate position following lastPP in the path
     * @return true if any potential path position has been found around module, and nextPP has been set
     */
    bool findNextPP(Cell3DPosition lastPP, Cell3DPosition &nextPP); 
     
    /**** GROW *****/
    list<Cell3DPosition> targetCells; //!< Ordered list of target positions to be filled during the growth phase    
    bool growing = false; //!< Indicates whether current module is currently moving to fill a target position during the growth phase
    /* bool grown = false; //!< Indicates whether current module is done with its growth and is currently filling a target position */
    /* bool firstGrowth = true; //!< Indicates whether the current growth movement is the first, it is necessary as we initialize the growth DFS-tree during the first growth */
    bool growthVisited = false; //!< Indicates whether the current module has been visited during this growth phase
    P2PNetworkInterface *growthParent = NULL; //!< The module's parent in the DFS-tree created during the growth phase
    Cell3DPosition goalPosition; //!< Position to be filled by current growth     
     
    /**
     * @brief Initializes the local variables used by the articulation points labelling algorithm
     */
    void APLabellingInitialization();
    /**
     * @brief Searches articulation points in the module graph using DFS
     */
    void APLabellingSearch();
    /**
     * @brief Initiates the articulation points labelling algorithm from source module
     */    
    void APLabellingStart();

    /**
     * @brief Initialize the path position trail for the tail module leading the current Melt
     */
    void initializeMeltPath();
         
    /**
     * @brief Searches the path positions of the module graph for a non-articulation point module DFS-style
     */   
    void findMobileModule();

    /**
     * @brief Sends a message to all nodes in the graph so that they get ready for the next AP labelling round
     */   
    void propagateGraphResetBFS();
     
    P2PNetworkInterface *getNextUnprocessedInterface();
    void resetDFSForLabelling();
    void resetDFSForSearching();
    void resetDFSFlags();
    
    /* bool amIArticulationPoint; //!< Label indicating whether current module is an articulation point */
    /* bool isTail; //!< Label indicating whether current module is tail of the decomposition line, there should be only one tail module at a given time */

    std::list<P2PNetworkInterface*> dfsQueue; //!< Stack of next interfaces to send message to DFS-style

    std::list<Cell3DPosition> parentPathPositions; //!< set of path positions given by the module's parent for the current module melt
    std::list<Cell3DPosition> myPathPositions; //!< set of path positions around the current module
    
    /**
     * @brief Reset the module's DFSQueue by re-adding all neighboring modules to it 
     */
    void resetDFSQueue();
    
    /**
     * @brief Called by the tail during the Melt phase. 
     *  Fills the module's path positions list with all free positions around module, starting by position right left to the tail, which is the next Melt's target.
     */    
    void initializePathPositions();

    /**
     * @brief Called by a DFS-ed module during the Melt phase. 
     *  Given its parent path and its position, the module tries to fill its path position list with all the free positions around it that are reachable from the parent's path.
     * @param pathPos pointer to the list of the parent's path positions
     * @param parentPos position of the module's parent on the lattice
     * @return true if there is at least one position in the path, false otherwise
     */    
    bool computePathPositions(list<Cell3DPosition>* pathPos, Cell3DPosition parentPos);

    /**
     * @brief Locate the root of the algorithm
     * i.e., find the left-most module in the whole ensemble (perhaps extend to lowest-leftmost for 3D)
     */
    void determineRoot();
    /**
     * @brief Initiates the Melt phase of the algorithm by the root of the system
     *  i.e., decomposition into an intermediate shape
     */
    void meltOneModule();
    
    /**
     * @brief Initiates the Sort phase of the algorithm
     */
    void sort();
    
    /**
     *  @brief Initiates the Grow phase of the algorithm
     *   i.e., grow the goal shape from the intermediate configuration
     */
    void grow();

    /**
     *  @brief Searches for a path to the next position to be filled during growth
     */
    void moveToGoal();
    
    /**
     * @brief Compares the fitness of the candidate root in argument and returns whether 
     it is fitter than current root
     * @param candidateRoot the position of the root to consider as new root
     * @return true if candidateRoot is fitter than current root, false otherwise */
    bool challengeRootFitness(Cell3DPosition& candidateRoot);
};

#endif /* MELTSORTGROWBLOCKCODE_H_ */
