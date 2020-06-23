/*
 * clock.h
 *
 *  Created on: 26 march 2015
 *      Author: andre
 */

#ifndef CONFIGSTAT_H_
#define CONFIGSTAT_H_

#include <list>
#include <string>

#include "../base/buildingBlock.h"
#include "../base/world.h"

using namespace std;

class SquareMatrix {
    int size;
    int *distances;

public:
    SquareMatrix(int s) {size = s; distances = new int[size*size];}
    ~SquareMatrix() {delete[] distances;}

    int getSize() {return size;}
    void set(int i, int j, int v) {distances[i*size + j] = v;}
    int get(int i, int j) {return distances[i*size + j];}
};

/**
 * Config Stat Computation
 */

class ConfigStat {
private:
    BaseSimulator::World *world;
    int size;
    SquareMatrix *distanceMatrix;
    int *eccentricity; // maximum distance
    int *closenessCentrality; // sum of the distances
    int *betweennessCentrality;
    int diameter;
    int radius;

    list<BaseSimulator::BuildingBlock*> center;
    list<BaseSimulator::BuildingBlock*> centroid;
    list<BaseSimulator::BuildingBlock*> betweennessCenter;

    void computeDistanceMatrix();
    void computeCenter();
    void computeCentroid();
    void computeBetweennessCenter();

    void print(string name,list<BaseSimulator::BuildingBlock*>& l);

    void deleteComputation();
    void initComputation();

public:

    ConfigStat(BaseSimulator::World *w);
    ~ConfigStat();

    void compute();

    int getDiameter();
    int getRadius();
    list<BaseSimulator::BuildingBlock*>& getCenter();
    list<BaseSimulator::BuildingBlock*>& getCentroid();
    list<BaseSimulator::BuildingBlock*>& getBetweennessCenter();

    void print();
};

#endif // CONFIGSTAT_H_
