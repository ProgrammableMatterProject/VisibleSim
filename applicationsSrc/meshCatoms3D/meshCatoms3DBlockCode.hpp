/*
 * meshCatoms3DBlockCode.h
 *
 *  Created on: 26/06/2018
 *      Author: pthalamy
 */

#ifndef MESHCATOMS3DBLOCKCODE_H_
#define MESHCATOMS3DBLOCKCODE_H_

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DMotionRules.h"
#include "rotation3DEvents.h"
#include "catoms3DBlock.h"

class MeshCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
private:
    static bool isOnXBranch(const Cell3DPosition &pos);
    static bool isOnYBranch(const Cell3DPosition &pos);    
    static bool isOnZBranch(const Cell3DPosition &pos);    
    static bool isOnRevZBranch(const Cell3DPosition &pos);    
    static bool isOnMinus45DegZBranch(const Cell3DPosition &pos);    
    static bool isOnPlus45DegZBranch(const Cell3DPosition &pos);
    static bool isInMesh(const Cell3DPosition &pos);
public:
    Scheduler *scheduler;
    World *world;
    Catoms3D::Catoms3DBlock *catom;
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
};

#endif /* MESHCATOMS3DBLOCKCODE_H_ */
