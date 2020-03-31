/**
 * @file   nodeDemoBlockCode.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:14:56 2019
 *
 * @brief
 *
 *
 */

#ifndef HEXANODEDEMOBLOCKCODE_H_
#define HEXANODEDEMOBLOCKCODE_H_

#include <list>
#include <set>
#include <unordered_set>
#include <map>
#include <climits>

#include "hexanodeBlockCode.h"
#include "hexanodeSimulator.h"
#include "hexanodeBlock.h"
#include "lattice.h"

using namespace Hexanode;

class HexanodeDemoBlockCode : public Hexanode::HexanodeBlockCode {
		HHLattice::Direction previousPivot;
public:
    Scheduler *scheduler;
    Hexanode::HexanodeBlock *node;
		HexanodeDemoBlockCode(Hexanode::HexanodeBlock *host);
    ~HexanodeDemoBlockCode();

    void startup() override;
		//void processLocalEvent(EventPtr pev) override;
		void onMotionEnd() override;

		void processReceivedMessage(MessagePtr msg, P2PNetworkInterface* sender);
		
		static BlockCode *buildNewBlockCode(BuildingBlock *host) {
			return (new HexanodeDemoBlockCode((HexanodeBlock*)host));
		}
		
};

#endif /* HEXANODEDEMOBLOCKCODE_H_ */
