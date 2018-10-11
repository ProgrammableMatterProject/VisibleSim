/**
 * @file   catoms3DRotationsBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Oct 10 14:12:52 2018
 * 
 * @brief  
 * 
 * 
 */


#ifndef CATOMS3DROTATIONSBLOCKCODE_H_
#define CATOMS3DROTATIONSBLOCKCODE_H_

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DMotionRules.h"
#include "rotation3DEvents.h"
#include "catoms3DBlock.h"
#include "cell3DPosition.h"

class Catoms3DRotationsBlockCode : public Catoms3D::Catoms3DBlockCode {
public:    
    Scheduler *scheduler;
    World *world;
    Lattice *lattice;
    Catoms3D::Catoms3DBlock *catom;

    // TargetCSG *target;
    Catoms3DRotationsBlockCode(Catoms3D::Catoms3DBlock *host);
    ~Catoms3DRotationsBlockCode();          
    
    /**
     * \brief Global message handler for this instance of the blockcode
     * \param msg Message received b
y the module
     * \param sender Connector that has received the message and hence that is connected to the sender */
    void processReceivedMessage(MessagePtr msg, P2PNetworkInterface* sender);
    
    void startup();
    void processLocalEvent(EventPtr pev);
    void onBlockSelected();
    void maFunc();

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new Catoms3DRotationsBlockCode((Catoms3DBlock*)host));
    }
};

#endif /* CATOMS3DROTATIONSBLOCKCODE_H_ */
    
