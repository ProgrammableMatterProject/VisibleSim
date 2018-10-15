/**
 * @file   messages.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 14:13:13 2018
 * 
 * @brief  
 * 
 * 
 */

#include <iostream>
#include <sstream>

#include "utils.h"

#include "teleportationEvents.h"
#include "rotation3DEvents.h"

#include "meshRuleMatcher.hpp"
#include "meshAssemblyBlockCode.hpp"
#include "meshAssemblyMessages.hpp"


void RequestTargetCellMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);    

    if (mabc.role == ActiveBeamTip) {
        // Forward message to coordinator
        P2PNetworkInterface* coordItf =
            mabc.catom->getInterface(mabc.coordinatorPos);
        VS_ASSERT_MSG(coordItf, "cannot find coordinator among neighbor interfaces");
        mabc.sendMessage(this->clone(), coordItf, MSG_DELAY_MC, 0);        
    } else if (mabc.role == Coordinator) {
        // A neighboring catom requested an objective, check needs an make decision based on their distance.
        // FIXME: For now, route towards horizontal growth
        // const Cell3DPosition& rPos = srcPos - mabc.catom->position;
        // short epd = mabc.getEntryPointDirectionForCell(rPos);

        const Cell3DPosition& tPos =
            mabc.targetForEntryPoint[mabc.getEntryPointDirectionForCell(srcPos)];

        // cout << "Spawnee Position: " << srcPos 
        //      << " -- Target Position: " << tPos
        //      << " -- Relative Position: " << rPos
        //      << " -- epd: " << epd << endl;
    
        // Send to requesting catom
        VS_ASSERT(destinationInterface->isConnected());
        mabc.sendMessage(new ProvideTargetCellMessage(tPos, srcPos),
                         destinationInterface, MSG_DELAY_MC, 0);
    } else {
        mabc.catom->setColor(BLACK);
        VS_ASSERT_MSG(false, "Non coordinator or active beam module should not have received a RequestTargetCellMessage");
    }
}

void ProvideTargetCellMessage::handle(BaseSimulator::BlockCode* bc) {
    MeshAssemblyBlockCode& mabc = *static_cast<MeshAssemblyBlockCode*>(bc);

    if (mabc.role == ActiveBeamTip) {
        // Forward message to coordinator
        P2PNetworkInterface* dstItf =
            mabc.catom->getInterface(dstPos);
        VS_ASSERT_MSG(dstItf, "cannot find destination among neighbor interfaces");
        mabc.sendMessage(this->clone(), dstItf, MSG_DELAY_MC, 0);        
    } else {
        const Cell3DPosition& adjustedSenderPos =
            mabc.catom->position[2] == mabc.meshSeedPosition[2] - 1 ?
            sourceInterface->hostBlock->position + Cell3DPosition(0,0,mabc.B)
            : sourceInterface->hostBlock->position;


        // cout << "sourceInterface->hostBlock->position: " <<sourceInterface->hostBlock->position
        //      << " -- adjustedPosition: " << adjustedSenderPos << endl;
        
        mabc.coordinatorPos = sourceInterface->hostBlock->position-mabc.incidentTipRelativePos[
            mabc.ruleMatcher->determineBranchForPosition(mabc.norm(adjustedSenderPos))];

        // cout << "mabc.coordinatorPos: " << mabc.coordinatorPos << " -- branch: "
        //      << mabc.ruleMatcher->determineBranchForPosition(mabc.norm(adjustedSenderPos))
        //      << " -- delegatePos: " << mabc.coordinatorPos + mabc.incidentTipRelativePos[mabc.ruleMatcher->determineBranchForPosition(mabc.norm(adjustedSenderPos))] << endl;

        mabc.targetPosition = tPos;

        Cell3DPosition nextHop;

        cout << "received tPos for module " << destinationInterface->hostBlock->blockId
             << ": " << tPos << endl;
    
        // Consider ack
        if (mabc.lattice->cellsAreAdjacent(mabc.catom->position, tPos)) {
            VS_ASSERT_MSG(mabc.lattice->isFree(tPos), "target branch position must be free!!!");
            nextHop = tPos;
        } else {
            // Determine position relative to branch that is being grown.
            /// If catom has been properly introduced, then it should be on one of the four bottom
            ///  connectors of the local coordinator. The simplest rule is to simply move along
            ///  the direction of the branch until reaching a neighboring position to the target;
            ///  this should always be possible.        
            const Cell3DPosition& relPos =
                mabc.catom->position - mabc.coordinatorPos;

            // Deduce next position
            BranchIndex bi = mabc.ruleMatcher->
                getBranchIndexForNonRootPosition(mabc.norm(tPos));

            cout << "Relative position to local coordinator: " <<  relPos
                 << " -- branch: " << bi << endl;
        
            if (bi > 3)
                nextHop = mabc.catom->position + mabc.ruleMatcher->getBranchUnitOffset(bi);

            else if (bi == ZBranch) {
                if (mabc.coordinatorPos == mabc.meshSeedPosition)
                    nextHop = mabc.catom->position + mabc.ruleMatcher->getBranchUnitOffset(bi);

            } else if (bi == RevZBranch) {
                if (mabc.ruleMatcher->isOnXOppBorder(mabc.norm(mabc.coordinatorPos))
                    and mabc.ruleMatcher->isOnYOppBorder(mabc.norm(mabc.coordinatorPos)))
                    // Mesh farthest corner
                    nextHop = mabc.catom->position + Cell3DPosition(-1,-1,2);
                else 
                    nextHop = mabc.catom->position + Cell3DPosition(0,0,1);

            } else if (bi == LeftZBranch) {
                if (mabc.ruleMatcher->isOnXBorder(mabc.norm(mabc.coordinatorPos))
                    or mabc.ruleMatcher->isOnYOppBorder(mabc.norm(mabc.coordinatorPos))) {
                    nextHop = mabc.catom->position + Cell3DPosition(0,-1,1);
                } else {
                    VS_ASSERT(false);
                }

            } else if (bi == RightZBranch) {
                if (mabc.ruleMatcher->isOnYBorder(mabc.norm(mabc.coordinatorPos))) {
                    nextHop = mabc.catom->position + Cell3DPosition(-1,-1,1);
                } else if (mabc.ruleMatcher->isOnXOppBorder(mabc.norm(mabc.coordinatorPos))) {
                    nextHop = mabc.catom->position + Cell3DPosition(0,0,1);
                } else {
                    VS_ASSERT(false);
                }

            } else {
                VS_ASSERT(false);
            }
        }

        try {
            mabc.console << "Rotating to " << nextHop << " from " << mabc.catom->position << "\n";
            mabc.scheduler->schedule(
                new Rotation3DStartEvent(getScheduler()->now(), mabc.catom, nextHop));
        } catch (const NoAvailableRotationPivotException& e_piv) {
            cerr << e_piv.what();
            cerr << "target position: " << nextHop << endl;
            mabc.catom->setColor(RED);
            VS_ASSERT(false);
        } catch (std::exception const& e) {
            cerr << "exception: " << e.what() << endl;
            VS_ASSERT(false);
        }


#ifdef INTERACTIVE_MODE
        awaitKeyPressed();
#endif

    // } else {
    //     mabc.catom->setColor(BLACK);
    //     VS_ASSERT_MSG(false, "Non coordinator or active beam module should not have received a ProvideTargetCellMessage");
    // }
    }
}
