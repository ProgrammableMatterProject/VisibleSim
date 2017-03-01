#include <iostream>
#include "reconfCatoms3DBlockCode.h"

#define WAIT_TIME 0

using namespace std;
using namespace Catoms3D;

string CSG_FILE = "data/mug.bc";

ReconfCatoms3DBlockCode::ReconfCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
	cout << "ReconfCatoms3DBlockCode constructor" << endl;
    scheduler = getScheduler();
	catom = (Catoms3DBlock*)hostBlock;

    reconf.setCatom(catom);
    neighbor.setCatom(catom);
    syncRequest.setCatom(catom);
    syncResponse.setCatom(catom);
}

ReconfCatoms3DBlockCode::~ReconfCatoms3DBlockCode() {
	cout << "ReconfCatoms3DBlockCode destructor" << endl;
}

void ReconfCatoms3DBlockCode::debug() {
    if (catom->blockId == 167) {
        neighbor.checkLineCompleted(reconf);
        if (reconf.needSync())
            catom->setColor(RED);
        std::this_thread::sleep_for(std::chrono::milliseconds(1000));
        syncRequest.syncLineSeedToLeft(167, 10, reconf, TO_PREVIOUS);
    }
    else 
    {
        if (catom->getInterface(0)->connectedInterface == NULL && catom->getInterface(6)->connectedInterface == NULL) {
            reconf.lineParent = catom->blockId;
            neighbor.checkLineCompleted(reconf);
            neighbor.addNeighborToLeft(buildNewBlockCode, reconf);
            neighbor.addNeighborToRight(buildNewBlockCode, reconf);
        }
        else {
            neighbor.sendMessageToGetNeighborInformation();
        }
    }
}

void ReconfCatoms3DBlockCode::startup() {
	if (catom->blockId==1) {
        CsgUtils::init(CSG_FILE);
        if (!CsgUtils::isInside(catom->position)) {
            catom->setColor(RED);
        }
	}
    bool DEBUG = true;
    if (DEBUG) {
        debug();
    } 
    else {
        // First catom of the line
        if (catom->getInterface(0)->connectedInterface == NULL && catom->getInterface(6)->connectedInterface == NULL) {
                reconf.lineParent = catom->blockId;
                neighbor.checkLineCompleted(reconf);
                neighbor.addNeighborToLeft(buildNewBlockCode, reconf);
                neighbor.addNeighborToRight(buildNewBlockCode, reconf);
        }
        else {
            neighbor.sendMessageToGetNeighborInformation();
        }
    }
    if (catom->blockId == 15) {
        cout << reconf.isLeftCompleted() << " " << reconf.isRightCompleted() << endl;
    }
    std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
}

void ReconfCatoms3DBlockCode::processLocalEvent(EventPtr pev) {
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
    case EVENT_NI_RECEIVE: {
      message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
        switch(message->id) {
            // Arriving catom asks neighbor catom for his informations.
            case NEW_CATOM_MSG_ID:
            {
                New_catom_response_message *msg = new New_catom_response_message;
                msg->lineParent = reconf.lineParent;
                msg->leftCompleted = reconf.isLeftCompleted();
                msg->rightCompleted = reconf.isRightCompleted();
                msg->numberSeedsLeft = reconf.numberSeedsLeft + (reconf.isSeed() ? 1 : 0);
                msg->numberSeedsRight = reconf.numberSeedsRight + (reconf.isSeed() ? 1 : 0);

                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, msg, message->destinationInterface));
                break;
            }
            // Arriving catom saves the information sent by neighbor catom that called him.
            // LEFT_SIDE_COMPLETED_MSG_ID and RIGHT can come before this message, so we should take attention to not lost data.
            case NEW_CATOM_RESPONSE_MSG_ID:
            {
                shared_ptr<New_catom_response_message> recv_message = static_pointer_cast<New_catom_response_message>(message);
                reconf.lineParent = recv_message->lineParent;
                if (recv_message->leftCompleted)
                    reconf.setLeftCompleted();
                if (recv_message->rightCompleted)
                    reconf.setRightCompleted();
                reconf.numberSeedsLeft = max(recv_message->numberSeedsLeft, reconf.numberSeedsLeft) ;
                reconf.numberSeedsRight = max(recv_message->numberSeedsRight, reconf.numberSeedsRight);

                std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));

                neighbor.checkLineCompleted(reconf);
                neighbor.addNeighborToLeft(buildNewBlockCode, reconf);
                neighbor.addNeighborToRight(buildNewBlockCode, reconf);

                if (reconf.isLeftCompleted() && reconf.isRightCompleted()) {
                    neighbor.tryAddNextLineNeighbor(buildNewBlockCode, reconf);
                }

                break;
            }
            case LEFT_SIDE_COMPLETED_MSG_ID:
            {
                reconf.setLeftCompleted();
                shared_ptr<Left_side_completed_message> recv_message = static_pointer_cast<Left_side_completed_message>(message);
                reconf.numberSeedsLeft = recv_message->numberSeedsLeft;

                if (catom->getInterface(catom->position.addX(1))->connectedInterface != NULL) {
                    Left_side_completed_message *msg = new Left_side_completed_message(reconf.numberSeedsLeft + (reconf.isSeed() ? 1 : 0));
                    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, msg, catom->getInterface(catom->position.addX(1))));
                }
                if (reconf.isRightCompleted()) 
                {
                    neighbor.tryAddNextLineNeighbor(buildNewBlockCode, reconf);
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
                break;
            }
            case RIGHT_SIDE_COMPLETED_MSG_ID:
            {
                reconf.setRightCompleted();
                shared_ptr<Right_side_completed_message> recv_message = static_pointer_cast<Right_side_completed_message>(message);
                reconf.numberSeedsRight = recv_message->numberSeedsRight;

                if (catom->getInterface(catom->position.addX(-1))->connectedInterface != NULL) {
                    Right_side_completed_message *msg = new Right_side_completed_message(reconf.numberSeedsRight + (reconf.isSeed() ? 1 : 0));
                    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 10, msg, catom->getInterface(catom->position.addX(-1))));
                }
                if (reconf.isLeftCompleted()) {
                    neighbor.tryAddNextLineNeighbor(buildNewBlockCode, reconf);
                }

                std::this_thread::sleep_for(std::chrono::milliseconds(WAIT_TIME));
                break;
            }
            case LOOKUP_NEIGHBOR_SYNC_MESSAGE_ID:
            {
                shared_ptr<Lookup_neighbor_sync_message> recv_message = static_pointer_cast<Lookup_neighbor_sync_message>(message);

                if (recv_message->side_direction == TO_LEFT)
                    syncRoutes[recv_message->requestCatomID].direction = DIRECTION_RIGHT;
                if (recv_message->side_direction == TO_RIGHT)
                    syncRoutes[recv_message->requestCatomID].direction = DIRECTION_LEFT;
                syncRequest.syncLineNeighborToLeft(recv_message->requestCatomID, recv_message->requestLine, reconf, recv_message->side_direction);
//                catom->setColor(LIGHTGREEN);
                break;
            }
            case LOOKUP_LINE_SYNC_MESSAGE_ID:
            {
                shared_ptr<Lookup_line_sync_message> recv_message = static_pointer_cast<Lookup_line_sync_message>(message);
                if (recv_message->lineDirection == TO_NEXT)
                    syncRoutes[recv_message->requestCatomID].direction = DIRECTION_DOWN;
                if (recv_message->lineDirection == TO_PREVIOUS) 
                    syncRoutes[recv_message->requestCatomID].direction = DIRECTION_UP;
                /*if (catom->blockId == lineParent && 
                        currentLine == recv_message->requestLine) {*/
                if (catom->blockId == 139) {
                    std::this_thread::sleep_for(std::chrono::milliseconds(1000));
                //    shared_ptr<Sync_response_message> recv_message = static_pointer_cast<Sync_response_message>(message);
                    syncResponse.response(recv_message->requestCatomID, syncRoutes[recv_message->requestCatomID].direction, true);
                }
                else {
                    syncRequest.syncLineSeedToLeft(recv_message->requestCatomID, recv_message->requestLine, reconf, recv_message->lineDirection);
                }
                catom->setColor(GREEN);
                std::this_thread::sleep_for(std::chrono::milliseconds(10));
                break;
            }
            case SYNC_RESPONSE_MESSAGE_ID:
            {
                catom->setColor(BLUE);
                shared_ptr<Sync_response_message> recv_message = static_pointer_cast<Sync_response_message>(message);
                if (recv_message->requestCatomID != catom->blockId) {
                    syncResponse.forwardResponse(recv_message, syncRoutes[recv_message->requestCatomID]);
                }
                else {
                    catom->setColor(BLACK);
                    neighbor.sendMessageToGetNeighborInformation();
                }
                std::this_thread::sleep_for(std::chrono::milliseconds(100));
            }
          }
      }
      break;
	}
}


BlockCode* ReconfCatoms3DBlockCode::buildNewBlockCode(BuildingBlock *host) {
    return (new ReconfCatoms3DBlockCode((Catoms3DBlock*)host));
}
