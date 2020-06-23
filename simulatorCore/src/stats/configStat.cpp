#include <map>
#include <climits>

#include "configStat.h"
#include "../events/scheduler.h"
#include "../comm/network.h"

using namespace std;
using namespace BaseSimulator;

ConfigStat::ConfigStat(BaseSimulator::World *w) {
    diameter = 0;
    radius = 0;
    size = 0;
    world = w;
    distanceMatrix = NULL;
    eccentricity = NULL;
    closenessCentrality = NULL;
    betweennessCentrality = NULL;
    compute();
}

void ConfigStat::deleteComputation() {
    delete[] eccentricity; eccentricity = NULL;
    delete[] closenessCentrality; closenessCentrality = NULL;
    delete[] betweennessCentrality; betweennessCentrality = NULL;
    delete distanceMatrix; distanceMatrix = NULL;
    center.clear();
    centroid.clear();
    betweennessCenter.clear();
    diameter = 0;
    radius = 0;
}

void ConfigStat::initComputation() {
    size = world->getMap().size()+1;
    distanceMatrix = new SquareMatrix(size);
    eccentricity = new int[size];
    closenessCentrality = new int[size];
    betweennessCentrality = new int[size];
}

ConfigStat::~ConfigStat() {
    deleteComputation();
}

int ConfigStat::getDiameter() {
    return diameter;
}

int ConfigStat::getRadius() {
    return radius;
}

list<BaseSimulator::BuildingBlock*>& ConfigStat::getCenter() {
    return center;
}

list<BaseSimulator::BuildingBlock*>& ConfigStat::getCentroid() {
    return centroid;
}

list<BaseSimulator::BuildingBlock*>& ConfigStat::getBetweennessCenter() {
    return betweennessCenter;
}


void ConfigStat::compute() {

    deleteComputation();
    initComputation();

    computeDistanceMatrix();
    computeCenter();
    computeCentroid();
    computeBetweennessCenter();
}

void ConfigStat::computeDistanceMatrix() {
    int size = distanceMatrix->getSize();
    bool adjacencyMatrix[size][size];
    int distTmp;

    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            adjacencyMatrix[i][j] = false;
            distanceMatrix->set(i, j, INT_MAX);
        }
    }

    map<bID, BuildingBlock*>::iterator bit;
    for (bit = world->getMap().begin() ; bit != world->getMap().end() ; bit++) {
        vector<P2PNetworkInterface*>::const_iterator niit;
        for (niit = bit->second->getP2PNetworkInterfaces().begin(); niit != bit->second->getP2PNetworkInterfaces().end(); niit++) {
            if ((*niit)->connectedInterface) {
                adjacencyMatrix[bit->second->blockId][(*niit)->connectedInterface->hostBlock->blockId] = true;
            }
        }
    }

    bool changed = false;
    for (int s = 1; s < size; s++) {
        distanceMatrix->set(s,s,0);
        for (int i = 1; i < size; i++ ) {
            changed = false;
            for (int u = 1; u < size; u++ ) {
                for (int v = 1; v < size; v++) {
                    if (adjacencyMatrix[u][v]) {
                        //if (distance[s][v] != UINT_MAX) {
                        if (distanceMatrix->get(s,u) != INT_MAX) {
                            //distTmp = distance[s][u] + 1;
                            distTmp = distanceMatrix->get(s,u) + 1;
                            //if (distTmp < distance[s][v]) {
                            //cout << "distTmp" <<
                            if (distTmp < distanceMatrix->get(s,v)) {
                                //cout << "set: " << s << "," << v << ": " << v << endl;
                                distanceMatrix->set(s,v,distTmp);
                                distanceMatrix->set(v,s,distTmp);
                                changed = true;
                                //distance[s][v] = distTmp;
                            }
                        }
                    }
                }
            }
            if(!changed) break;
        }
    }
}

void ConfigStat::computeCenter() {
    radius = INT_MAX;
    diameter = 0;

    for (int i = 1; i < size; i++) {
        eccentricity[i] = 0;
        for (int j = 1; j < size; j++) {
            eccentricity[i] = max(eccentricity[i],distanceMatrix->get(i,j));
        }
        radius = min(radius, eccentricity[i]);
        diameter = max(diameter, eccentricity[i]);
    }

    for (int i = 1; i < size; i++) {
        if (eccentricity[i] == radius) {
            center.push_back(world->getBlockById(i));
        }
    }
}

void ConfigStat::computeCentroid() {
    int minSum = INT_MAX;
    int maxSum = 0;

    for (int i = 1; i < size; i++) {
        closenessCentrality[i] = 0;
        for (int j = 1; j < size; j++) {
            closenessCentrality[i] += distanceMatrix->get(i,j);
        }
        minSum = min(closenessCentrality[i], minSum);
        maxSum = max(closenessCentrality[i], maxSum);
    }

    for (int i = 1; i < size; i++) {
        if (closenessCentrality[i] == minSum) {
            centroid.push_back(world->getBlockById(i));
        }
    }

    (void) maxSum;
}

void ConfigStat::computeBetweennessCenter() {
    // TODO
}

void ConfigStat::print(string name, list<BaseSimulator::BuildingBlock*>& l) {
    cout << name <<":";
    for (list<BaseSimulator::BuildingBlock*>::iterator it=l.begin(); it != l.end(); it++) {
        cout << ' ' << (*it)->blockId << "(eccentricity:" << eccentricity[(*it)->blockId]<< ", closeness centrality:" <<  closenessCentrality[(*it)->blockId] << ")";
    }
    cout << endl;
}

void ConfigStat::print() {
    cout << "Diameter: " << diameter << "," << "radius: " << radius << endl;
    print("Center", center);
    print("Centroid", centroid);
    print("Betweenness", betweennessCenter);
}
