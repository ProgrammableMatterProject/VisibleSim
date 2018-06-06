#include "syncPlaneNode.h"

SyncPlane_node::SyncPlane_node(int bId, int pN) {
    blockId = bId;
    planeNumber = pN;
    completed = false;
}

void SyncPlane_node::add(SyncPlane_node *node, SyncPlane_node *parent) {
    if (this == parent) {
        children.push_back(node);
    }
    else {
        for (int i = 0; i < children.size(); i++)
            children[i]->add(node, parent);
    }
}

void SyncPlane_node::remove(SyncPlane_node *node, SyncPlane_node *parent) {
    if (this == parent) {
        for (int i = 0; i < children.size(); i++)
            if (children[i] == node)
                children.erase(children.begin()+i);
    }
    else {
        for (int i = 0; i < children.size(); i++)
            children[i]->remove(node, parent);
    }
}

void SyncPlane_node::print() {
    cout << "BlockId = " << blockId << " completed = " << completed << endl;
    for (int i = 0; i < children.size(); i++) {
        children[i]->print();
    }
    cout << "end = " << blockId << endl;
}

void SyncPlane_node::setCompleted() {
    completed = true;
}

int SyncPlane_node::isOk(int pNumber) {
    int result = isOkInternal(pNumber);
    if (result == INF)
        return 0;
    return result;
}

int SyncPlane_node::isOkInternal(int pNumber) {
    if ((planeNumber < pNumber) && !completed)
        return 0;
    else if (pNumber == planeNumber && !completed)
        return blockId;
    else {
        int result = INF;
        for (int i = 0; i < children.size(); i++)
            result = min(result, children[i]->isOkInternal(pNumber));

        return result;
    }
    return INF;
}

int SyncPlane_node::canContinue(int pNumber) {
    if (pNumber == planeNumber && !completed) {
        return blockId;
    }
    else {
        for (int i = 0; i < children.size(); i++) {
            int blockId = children[i]->canContinue(pNumber);
            if (blockId != 0)
                return blockId;
        }
    }
    return 0;
}
