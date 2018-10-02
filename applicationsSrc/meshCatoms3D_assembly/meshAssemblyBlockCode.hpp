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

#include "messages.hpp"
#include "meshRuleMatcher.hpp"

#define IT_MODE_TILEROOT_ACTIVATION 1 

class MeshAssemblyBlockCode : public Catoms3D::Catoms3DBlockCode {
private:
public:
    static const uint B = 6;
    static uint X_MAX, Y_MAX, Z_MAX; // const

    static const bool ASSEMBLE_SCAFFOLD = true;
    static const bool NOTIFY_SCAFFOLD_COMPLETION = true;
    static Cell3DPosition MeshSeedPosition; //const
    
    Scheduler *scheduler;
    World *world;
    Lattice *lattice;
    Catoms3D::Catoms3DBlock *catom;
    MeshCoating::MeshRuleMatcher *ruleMatcher;
    
    Cell3DPosition goalPosition;
    std::array<uint, 6> catomReqByBranch = {0,0,0,0,0,0};
    std::array<bool, 6> fedCatomOnLastRound = { false, false, false, false, false, false };
    std::array<Cell3DPosition*, 6> openPositions = {NULL, NULL, NULL, NULL, NULL, NULL};

    // TargetCSG *target;
    MeshAssemblyBlockCode(Catoms3D::Catoms3DBlock *host);
    ~MeshAssemblyBlockCode();          
    
    /**
     * \brief Global message handler for this instance of the blockcode
     * \param msg Message received by the module
     * \param sender Connector that has received the message and hence that is connected to the sender */
    void processReceivedMessage(MessagePtr msg, P2PNetworkInterface* sender);
    
    void startup();
    void processLocalEvent(EventPtr pev);
    void onBlockSelected();

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new MeshAssemblyBlockCode((Catoms3DBlock*)host));
    }

    static const Cell3DPosition normalize_pos(const Cell3DPosition& pos);
    void updateOpenPositions();
        
/**
     * \brief Uses central planning to find a path to a destination cell and follow it through rotating motions
     * \param dest Destination lattice position
     * \return true if a motion path has been found, false otherwise?? */
    bool moveToPosition(const Cell3DPosition& dest);
    bool moveToPosition_rec(const Cell3DPosition& nextHop);

    bool moduleInSpanningTree(const Cell3DPosition& pos);
    bool isMeshRoot(const Cell3DPosition& pos);

    bool checkOrthogonalIncidentBranchCompletion(const Cell3DPosition& pos);
};

#endif /* MESHCATOMS3DBLOCKCODE_H_ */
    
