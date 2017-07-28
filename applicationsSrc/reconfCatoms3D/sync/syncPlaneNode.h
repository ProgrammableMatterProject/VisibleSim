/*
 *  SyncPlaneNode.h
 *
 *  Created on: 22 July 2017
 *  Author: Thadeu
 */

#ifndef SYNCPLANENODE_H_
#define SYNCPLANENODE_H_

#include <iostream>
#include <vector>
#define INF 99999

using namespace std;

class SyncPlane_node {
private:
    vector<int> canContinue(vector<int> &vec, int pNumber);

public:
    int planeNumber;
    int blockId;
    vector<SyncPlane_node*> children;
    bool completed;

    SyncPlane_node(int bId, int pN);

    void add(SyncPlane_node *node, SyncPlane_node *parent);
    void remove(SyncPlane_node *node, SyncPlane_node *parent);
    void print();
    void setCompleted();
    int isOk(int pNumber);

    // return a list of ids of blocks that can continue
    vector<int> canContinue(int pNumber);
};

#endif /* SyncPlaneNode.h */
