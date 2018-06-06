#include "syncPlaneManager.h"

SyncPlaneManager::SyncPlaneManager(Catoms3D::Catoms3DBlock *c, Reconf *r, SyncPlane *sp, Neighborhood *nh, NeighborMessages *nm) {
    this->catom = c;
    this->reconf = r;
    this->syncPlane = sp;
    this->neighborhood = nh;
    this->neighborMessages = nm;
}

SyncPlaneManager::~SyncPlaneManager() {
}

void SyncPlaneManager::planeFinished()
{
    neighborMessages->sendMessagePlaneFinished();

    if (reconf->isPlaneParent) {
        if (reconf->syncPlaneNodeParent != NULL)
            reconf->syncPlaneNodeParent->setCompleted();
        planeFinishedAck();
    }
}

void SyncPlaneManager::planeFinishedAck()
{
    if (!reconf->isPlaneCompleted) {
        if (syncPlane->isSeed()) {
            setSeedNextPlaneCentralized();
            //tryAddNextPlane();
        }
        neighborMessages->sendMessagePlaneFinishedAck();
    }
}
void SyncPlaneManager::tryAddNextPlane()
{
    if (syncPlane->isSeed()) {
        if (SyncPlane_node_manager::root->isOk(catom->position[2]+1) == (int)catom->blockId) {
            neighborhood->addNeighborToNextPlane();
        }
    }
}

void SyncPlaneManager::setSeedNextPlaneCentralized()
{
    //catom->setColor(WHITE);
    neighborhood->addNeighborToNextPlane();
    reconf->syncPlaneNode = new SyncPlane_node(catom->blockId, catom->position[2]+1);
    SyncPlane_node_manager::root->add(reconf->syncPlaneNode, reconf->syncPlaneNodeParent);
}
