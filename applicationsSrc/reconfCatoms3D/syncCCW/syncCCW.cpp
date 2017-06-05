#include "syncCCW.h"
#include "catoms3DWorld.h"

// Starting at South
vector<pair<int, int>> ccw_order = { {0,-1}, {1,0}, {0,1}, {-1,0}};
vector<pair<int, int>> cw_order = { {0,-1}, {-1,0}, {0,1}, {1,0}};

SyncCCW::SyncCCW(Catoms3D::Catoms3DBlock *c, Reconf *r) : catom(c), reconf(r)
{
}

void SyncCCW::sync() {
    shared_ptr<SyncCCW_message> message(new SyncCCW_message(1, catom->position.addX(-1).addY(-1)));
    handleMessage(message);
}

void SyncCCW::response() {
    shared_ptr<SyncCCW_response_message> message(new SyncCCW_response_message(1, catom->position.addX(1).addY(1)));
    handleResponseMessage(message);
}

bool SyncCCW::canContinueLeftSeed() {
    int oldIdx = 0;
    int nTurns = 0;
    Cell3DPosition currentPos = catom->position;

    for (int i = 0; i < 4; i++) {
        int idx = (((oldIdx+i-1)%4)+4)%4;
        Cell3DPosition nextPos = currentPos.addX(cw_order[idx].first)
                                          .addY(cw_order[idx].second);
        if (BlockCode::target->isInTarget(nextPos)) {
            if (i == 0)
                nTurns++;
            else if (i == 2)
                nTurns--;
            else if (i == 3)
                nTurns -= 2;
            oldIdx = idx;
            currentPos = nextPos;
            break;
        }
    }

    while(currentPos != catom->position) {
        for (int i = 0; i < 4; i++) {
            int idx = (((oldIdx+i-1)%4)+4)%4;
            Cell3DPosition nextPos = currentPos.addX(cw_order[idx].first)
                                              .addY(cw_order[idx].second);
            if (BlockCode::target->isInTarget(nextPos)) {
                if (i == 0) {
                    nTurns++;
                }
                else if (i == 2)
                    nTurns--;
                else if (i == 3)
                    nTurns -= 2;
                oldIdx = idx;
                currentPos = nextPos;
                break;
            }
        }
    }
    cout << "catom id = " << catom->blockId << " nturn = " << nTurns << endl;
    catom->setColor(BLUE);
    if (nTurns > 0) return false;
    return true;
}

void SyncCCW::handleMessage(shared_ptr<SyncCCW_message> message) {
    for (int i = 0; i < 4; i++) {
        int idx = (((message->idx+i-1)%4)+4)%4;
        Cell3DPosition pos = catom->position.addX(ccw_order[idx].first)
                                            .addY(ccw_order[idx].second);
        P2PNetworkInterface *p2p = catom->getInterface(pos);
        if (p2p->isConnected()) {
            SyncCCW_message *msg = new SyncCCW_message(idx, message->goal);
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 10000, msg, p2p));
            break;
        }
    }
}

void SyncCCW::handleResponseMessage(shared_ptr<SyncCCW_response_message> message) {
    for (int i = 0; i < 4; i++) {
        int idx = (((message->idx+i-1)%4)+4)%4;
        Cell3DPosition pos = catom->position.addX(cw_order[idx].first)
                                            .addY(cw_order[idx].second);
        P2PNetworkInterface *p2p = catom->getInterface(pos);
        if (p2p->isConnected()) {
            SyncCCW_response_message *msg = new SyncCCW_response_message(idx, message->goal);
            getScheduler()->schedule(new NetworkInterfaceEnqueueOutgoingEvent(getScheduler()->now() + 10000, msg, p2p));
            break;
        }
    }
}
