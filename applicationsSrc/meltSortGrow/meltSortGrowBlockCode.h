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

#include "rotation3DEvents.h"
#include "catoms3DBlock.h"

#define MSG_ROOT_UPDATE 0011
#define MSG_ROOT_CONFIRM 0012
#define MSG_ROOT_NCONFIRM 0013

#define MSG_MELT_LABEL_AP 0020 // Label articulation points
#define MSG_MELT_LABEL_AP_DONE 0021 // Notify parent of labelling completion
#define MSG_MELT_FIND_MOBILE_MODULE 0022
#define MSG_MELT_FIND_MOBILE_MODULE_YUP 0023 // Notify parent we're on the move!
#define MSG_MELT_FIND_MOBILE_MODULE_NOPE 0024 // Notify parent we couldn't find non AP (Convergence condition)
#define MSG_MELT_UPDATE_TAIL 0025
#define MSG_MELT_UPDATE_TAIL_ACK 0026

#define MSG_MELT_APL_START 0030
#define MSG_MELT_APL_TOKEN 0031
#define MSG_MELT_APL_ECHO 0032
#define MSG_MELT_APL_VISITED 0033

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

private:
    /* Root Election Variables */
    Cell3DPosition currentRootPosition;
    unsigned int expectedConfirms;
    P2PNetworkInterface *parent;     //!< Connector connected to the module to confirm to
    std::list<P2PNetworkInterface*> children; //!< List of children in the tree
    bool electionClosed;


    /**** MELT *****/

    /* Articulation Points (AP) Labelling */
    /* See: Ahuja, Mohan, and Yahui Zhu. “An Efficient Distributed Algorithm for Finding Articulation Points, Bridges, and Biconnected Components in Asynchronous Networks.” In Foundations of Software Technology and Theoretical Computer Science, 99–108. Springer, 1989. */
    int dfn = 0; //!< Depth-First number (order in which a DFS first visited this node)
    int lDfn = 0; //!< Lowest DFN reachable from tis node using a path of descendants followed by at most one back edge
    int dfnCnt = 0; //!< DFN counter at this node
    int minDfn = INT_MAX; //!< Minimum FDN received by node from its ancestors other than its father
    P2PNetworkInterface *minSdr; //!< Id of the node which sent the minDfn
    enum APLState { Inactive, Active }; //!< Possible node states in AP labelling algorithms
    APLState state = APLState::Inactive;
    P2PNetworkInterface *father = NULL; //!< Connector connected to the node's father
    std::unordered_set<P2PNetworkInterface*> sons; //!< The node's children
    bool source = false; //!< Indicates whether module is source of the DFS
    bool bridge = false; //!< Indicates whether edge with father is a bridge
    bool articulationPoint = false; //!< Indicates whether node is an AP
    std::vector<P2PNetworkInterface*> neighbors; //!< Active neighbors around modul
    std::map<P2PNetworkInterface*, bool> flag; //!< flags for each neighbors when the node knows that they have been visited
    /**
     * @brief Initializes the local variables used by the articulation points labelling algorithm
     */
    void APLabellingInitialization();
    void APLabellingSearch();
    
    bool amIArticulationPoint; //!< Label indicating whether current module is an articulation point
    bool isTail; //!< Label indicating whether current module is tail of the decomposition line, there should be only one tail module at a given time

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
     * @brief Compares the fitness of the candidate root in argument and returns whether 
     it is fitter than current root
     * @param candidateRoot the position of the root to consider as new root
     * @return true if candidateRoot is fitter than current root, false otherwise */
    bool challengeRootFitness(Cell3DPosition& candidateRoot);
};

#endif /* MELTSORTGROWBLOCKCODE_H_ */
