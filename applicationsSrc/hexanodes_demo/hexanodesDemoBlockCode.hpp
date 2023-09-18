/**
 * @file   nodeDemoBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:14:56 2019
 *
 * @brief
 *
 *
 */

#ifndef HEXANODESDEMOBLOCKCODE_H_
#define HEXANODESDEMOBLOCKCODE_H_

#include <list>
#include <set>
#include <unordered_set>
#include <map>
#include <climits>

#include "robots/hexanodes/hexanodesBlockCode.h"
#include "robots/hexanodes/hexanodesSimulator.h"
#include "robots/hexanodes/hexanodesBlock.h"
#include "grid/lattice.h"

using namespace Hexanodes;

class HexanodesDemoBlockCode : public Hexanodes::HexanodesBlockCode {
public:
    Scheduler *scheduler;
    Hexanodes::HexanodesBlock *node;
    inline static size_t nMotions = 0;

    HexanodesDemoBlockCode(Hexanodes::HexanodesBlock *host);
    ~HexanodesDemoBlockCode() override {};

    void startup() override;
    //void processLocalEvent(EventPtr pev) override;
    void onMotionEnd() override;
    string onInterfaceDraw() override;

    void processReceivedMessage(MessagePtr msg, P2PNetworkInterface* sender);

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new HexanodesDemoBlockCode((HexanodesBlock*)host));
    }

};

#endif /* HEXANODESDEMOBLOCKCODE_H_ */
