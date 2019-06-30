/**
 * @file   nodeDemoBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:14:56 2019
 *
 * @brief
 *
 *
 */

#ifndef NODEDEMOBLOCKCODE_H_
#define NODEDEMOBLOCKCODE_H_

#include <list>
#include <set>
#include <unordered_set>
#include <map>
#include <climits>

#include "nodeBlockCode.h"
#include "nodeSimulator.h"
#include "nodeBlock.h"
#include "lattice.h"

using namespace Node;

class NodeDemoBlockCode : public Node::NodeBlockCode {
    int level = -1;
    bool hasTopNeighbor = false;
    bool hasBottomNeighbor = false;
		SLattice::Direction previousPivot;
public:
    Scheduler *scheduler;
    Node::NodeBlock *node;
    NodeDemoBlockCode(Node::NodeBlock *host);
    ~NodeDemoBlockCode();

    void startup() override;
    void processLocalEvent(EventPtr pev) override;
		void motionEnd();
		

#define LEVEL_MSG 0x1 // Message ID, for identification in processReceivedMessage
    void processReceivedMessage(MessagePtr msg, P2PNetworkInterface* sender);

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new NodeDemoBlockCode((NodeBlock*)host));
    }

    void setLevel(int lvl);
    void handle_levelMessage(std::shared_ptr<MessageOf<int>> msg,
                             P2PNetworkInterface *sender);
    bool isTopInterface(const P2PNetworkInterface* itf) const;
    bool isBottomInterface(const P2PNetworkInterface* itf) const;
    void updateNeighborLevels();
    void updateRainbowState();
};

#endif /* NODEDEMOBLOCKCODE_H_ */
