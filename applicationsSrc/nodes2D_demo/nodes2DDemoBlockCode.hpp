/**
 * @file   nodes2DDemoBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:14:56 2019
 *
 * @brief
 *
 *
 */

#ifndef NODES2DDEMOBLOCKCODE_H_
#define NODES2DDEMOBLOCKCODE_H_

#include <list>
#include <set>
#include <unordered_set>
#include <map>
#include <climits>

#include "robots/nodes2D/nodes2DBlockCode.h"
#include "robots/nodes2D/nodes2DSimulator.h"
#include "robots/nodes2D/nodes2DBlock.h"
#include "grid/lattice.h"

using namespace Nodes2D;

class Nodes2DDemoBlockCode : public Nodes2D::Nodes2DBlockCode {
    int level = -1;
    bool hasTopNeighbor = false;
    bool hasBottomNeighbor = false;
        SLattice::Direction previousPivot;
public:
    Scheduler *scheduler;
    Nodes2D::Nodes2DBlock *nodes2D;
    Nodes2DDemoBlockCode(Nodes2D::Nodes2DBlock *host);
    ~Nodes2DDemoBlockCode();

    void startup() override;
    void processLocalEvent(EventPtr pev) override;
        void motionEnd();


#define LEVEL_MSG 0x1 // Message ID, for identification in processReceivedMessage
    void processReceivedMessage(MessagePtr msg, P2PNetworkInterface* sender);

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new Nodes2DDemoBlockCode((Nodes2DBlock*)host));
    }

    void setLevel(int lvl);
    void handle_levelMessage(std::shared_ptr<MessageOf<int>> msg,
                             P2PNetworkInterface *sender);
    bool isTopInterface(const P2PNetworkInterface* itf) const;
    bool isBottomInterface(const P2PNetworkInterface* itf) const;
    void updateNeighborLevels();
    void updateRainbowState();
};

#endif /* NODES2DDEMOBLOCKCODE_H_ */
