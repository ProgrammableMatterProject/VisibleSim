/**
 * @file   meshCatoms3DBlockCode_mvmt.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 13:47:48 2018
 * 
 * @brief  
 * 
 * 
 */


#ifndef MESHCATOMS3DBLOCKCODE_H_
#define MESHCATOMS3DBLOCKCODE_H_

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DMotionRules.h"
#include "rotation3DEvents.h"
#include "catoms3DBlock.h"

#include "catoms3DMotionEngine.hpp"
#include "messages.hpp"
#include "meshSpanningTree.hpp"

#define MSG_DELAY_MC 5000

class MeshCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
private:
public:
    static const uint B = 6;
    static uint X_MAX;
    static uint Y_MAX;
    
    Scheduler *scheduler;
    World *world;
    Lattice *lattice;
    Catoms3D::Catoms3DBlock *catom;
    Catoms3DMotionEngine* engine;
    MeshSpanningTree::MeshSpanningTreeRuleMatcher *ruleMatcher;
    static bool skipMeshInit;
    Cell3DPosition goalPosition;

    Cell3DPosition posTileAwaitingPlacement;
    
    P2PNetworkInterface* stParent = NULL;
    uint expectedConfirms = 0;
    uint numberExpectedAcksFromSubTree = 13;
    
    // TargetCSG *target;
    MeshCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
    ~MeshCatoms3DBlockCode();          
    
    /**
     * \brief Global message handler for this instance of the blockcode
     * \param msg Message received by the module
     * \param sender Connector that has received the message and hence that is connected to the sender */
    void processReceivedMessage(MessagePtr msg, P2PNetworkInterface* sender);
    
    void startup();
    void processLocalEvent(EventPtr pev);

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new MeshCatoms3DBlockCode((Catoms3DBlock*)host));
    }

/**
     * \brief Uses central planning to find a path to a destination cell and follow it through rotating motions
     * \param dest Destination lattice position
     * \return true if a motion path has been found, false otherwise?? */
    bool moveToPosition(const Cell3DPosition& dest);
    bool moveToPosition_rec(const Cell3DPosition& nextHop);

    bool moduleInSpanningTree(const Cell3DPosition& pos);
    bool isMeshRoot(const Cell3DPosition& pos);

    void triggerMeshTraversalProcess();
    bool checkOrthogonalIncidentBranchCompletion(const Cell3DPosition& pos);
};

#endif /* MESHCATOMS3DBLOCKCODE_H_ */
    
