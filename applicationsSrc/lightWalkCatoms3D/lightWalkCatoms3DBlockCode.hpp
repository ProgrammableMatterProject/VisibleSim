/**
 * @file   lightWalkCatoms3DBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Mon Dec 10 15:27:23 2018
 *
 * @brief
 *
 *
 */

#ifndef LIGHTWALKCATOMS3DBLOCKCODE_H_
#define LIGHTWALKCATOMS3DBLOCKCODE_H_

#include <queue>

#include "robots/catoms3D/catoms3DBlockCode.h"
#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DMotionRules.h"
#include "robots/catoms3D/catoms3DRotationEvents.h"
#include "robots/catoms3D/catoms3DBlock.h"
#include "grid/cell3DPosition.h"

#include "lightWalkCatoms3DMessages.hpp"

/*
 * The goal of this blockcode is to have a certain number of modules move in along a line in parallel while leaving a gap between themselves (i.e., coordinating their motions so as to avoid potential collisions). When a catom reaches the end of the line, it completes it by taking its head position.
 */
class LightWalkCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
private:
public:
    Scheduler *scheduler;
    World *world;
    Lattice *lattice;
    Catoms3D::Catoms3DBlock *catom;

    bool initialized = false;

    LightWalkCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
    ~LightWalkCatoms3DBlockCode();

    /**
     * \brief Global message handler for this instance of the blockcode
     * \param msg Message received b
y the module
     * \param sender Connector that has received the message and hence that is connected to the sender */
    void processReceivedMessage(MessagePtr msg, P2PNetworkInterface* sender);

    void startup() override;
    void processLocalEvent(EventPtr pev) override;
    void onBlockSelected() override;


    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new LightWalkCatoms3DBlockCode((Catoms3DBlock*)host));
    }

    static const int ZLINE;

    bool greenLightIsOn = true;
    bool moduleAwaitingGo = false;
    Cell3DPosition awaitingModulePos = Cell3DPosition(-1, -1, -1);
    Cell3DPosition actuationTargetPos;

    bool relocated;
    bool rotating;
    Cell3DPosition targetPos;

    bool hasLeftNeighbor() const;
    bool hasReachedLineTail() const;
    bool isLineTip() const;
    bool isAdjacentToPosition(const Cell3DPosition& pos) const;

    // NOTE: what if there is more than 1?
    Catoms3DBlock* findTargetLightAmongNeighbors(const Cell3DPosition& targetPos) const;
    void setGreenLightAndResumeFlow();
};

#endif /* LIGHTWALKCATOMS3DBLOCKCODE_H_ */
