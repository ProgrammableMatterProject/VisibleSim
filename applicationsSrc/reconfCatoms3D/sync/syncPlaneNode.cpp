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
    cout << "BlockId = " << blockId << endl;
    for (int i = 0; i < children.size(); i++) {
        cout << "and ";
        children[i]->print();
    }
}

void SyncPlane_node::setCompleted() {
    completed = true;
}

int SyncPlane_node::isOk(int pNumber) {
    if ((planeNumber < pNumber) && !completed)
        return 0;
    else if (pNumber == planeNumber && !completed)
        return blockId;
    else {
        int result = INF;
        for (int i = 0; i < children.size(); i++)
            result = min(result, children[i]->isOk(pNumber));

        return result;
    }
    return INF;
}

vector<int> SyncPlane_node::canContinue(int pNumber) {
    vector<int> vec;
    return canContinue(vec, pNumber);
}

// return a list of ids of blocks that can continue
vector<int> SyncPlane_node::canContinue(vector<int> &vec, int pNumber) {
    if (pNumber == planeNumber && !completed) {
        vec.push_back(blockId);
    }
    else {
        for (int i = 0; i < children.size(); i++) {
            children[i]->canContinue(vec, pNumber);
        }
    }
    return vec;
}
