/*
 * meltSortGrowBlockCode.h
 *
 *  Created on: 07/11/17
 *      Author: pthalamy
 */

#ifndef MELTSORTGROWBLOCKCODE_H_
#define MELTSORTGROWBLOCKCODE_H_

#include <set>

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

class MeltSortGrowBlockCode : public Catoms3D::Catoms3DBlockCode {
private:
    /* Root Election Variables */
    Cell3DPosition currentRootPosition;
    unsigned int expectedConfirms;
    P2PNetworkInterface *parent;     //!< Connector connected to the module to confirm to
    std::list<P2PNetworkInterface*> children; //!< List of children in the tree
    bool electionClosed;

    /* Melt Variables */
public:
    struct posCmp
    {
        bool operator()(const Cell3DPosition p1, const Cell3DPosition p2) const
            {
                return p1 < p2;
            }
    };
private:
    std::list<P2PNetworkInterface*> dfsQueue; //!< Stack of next interfaces to send message to DFS-style
    Cell3DPosition meltTargetPosition; //!< Position to be filled by the module for the melt phase
    std::list<Cell3DPosition> parentPathPositions; //!< set of path positions given by the module's parent for the current module melt
    std::list<Cell3DPosition> myPathPositions; //!< set of path positions around the current module

    
    bool amIArticulationPoint;
    bool isTail;
    void resetDFSQueue();
    std::set<Cell3DPosition, posCmp>*
         computePathPositions(std::set<Cell3DPosition, posCmp> *path);
    const Cell3DPosition computeNextMove(std::set<Cell3DPosition, posCmp> *path);

    /* virtual bool getAttribute(const string &att,ostringstream &sout); */

    // Locate the root of the algorithm
    // i.e., find the left-most module in the whole ensemble (perhaps extend to lowest-leftmost for 3D)
    void determineRoot();
    // Initiates the Melt phase of the algorithm by the root of the system
    //  i.e., decomposition into an intermediate shape
    void meltOneModule();
    
    // Initiates the Sort phase of the algorithm
    void sort();
    // Initiates the Grow phase of the algorithm
    //  i.e., grow the goal shape from the intermediate configuration
    void grow();
/**
 * @brief Compares the fitness of the candidate root in argument and returns whether 
           it is fitter than current root
 * @param candidateRoot the position of the root to consider as new root
 * @return true if candidateRoot is fitter than current root, false otherwise */
    bool challengeRootFitness(Cell3DPosition& candidateRoot);

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
};

/* /\** */
/*  * @brief Global message handler that calls the specific message handler of the blockcode */
/*  *         receiving the message */
/*  * @param bc BlockCode of the module receiving the message m */
/*  * @param msg Message received by the module */
/*  * @param sender Connector that has received the message and hence that is connected to the sender *\/ */
/* static void _processReceivedMessage(BlockCode *bc, MessagePtr msg, P2PNetworkInterface *sender); */


#endif /* MELTSORTGROWBLOCKCODE_H_ */
