/**
 * @file   c2dDemoBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 09:50:40 2019
 *
 * @brief
 *
 *
 */

#ifndef C2DDEMOBLOCKCODE_H_
#define C2DDEMOBLOCKCODE_H_

#include <list>
#include <set>
#include <unordered_set>
#include <map>
#include <climits>

#include "robots/catoms2D/catoms2DBlockCode.h"
#include "robots/catoms2D/catoms2DSimulator.h"
#include "robots/catoms2D/catoms2DBlock.h"

using namespace Catoms2D;

class C2dDemoBlockCode : public Catoms2D::Catoms2DBlockCode {
    int level = -1;
    bool hasTopNeighbor = false;
    bool hasBottomNeighbor = false;
public:
    Scheduler *scheduler;
    Catoms2D::Catoms2DBlock *catom;
    C2dDemoBlockCode(Catoms2D::Catoms2DBlock *host);
    ~C2dDemoBlockCode();

    void startup() override;
    void processLocalEvent(EventPtr pev) override;

#define LEVEL_MSG 0x1 // Message ID, for identification in processReceivedMessage
    void processReceivedMessage(MessagePtr msg, P2PNetworkInterface* sender);

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new C2dDemoBlockCode((Catoms2DBlock*)host));
    }

    void setLevel(int lvl);
    void handle_levelMessage(std::shared_ptr<MessageOf<int>> msg,
                             P2PNetworkInterface *sender);
    bool isTopInterface(const P2PNetworkInterface* itf) const;
    bool isBottomInterface(const P2PNetworkInterface* itf) const;
    void updateNeighborLevels();
};

#endif /* C2DDEMOBLOCKCODE_H_ */
