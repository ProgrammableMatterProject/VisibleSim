//
// Created by bpiranda on 19/10/2023.
//
#include <cstring>
#include <iostream>
#include "SupportPolygon.h"

using namespace std;

SupportPolygon::SupportPolygon(const SupportPolygon &src) {
    Nmax = src.Nmax;
    N = src.N;
    tabVertices = new Vector2D[Nmax];
    memcpy(tabVertices,src.tabVertices,Nmax*sizeof(Vector2D));
}

void SupportPolygon::addLastVertex(const Vector2D &v) {
    //cout << "Add (" << N << "<" << Nmax-1 << "):" << v.x << "," << v.y << endl;
    if (N + 1 == Nmax) { //resize
        Nmax += 10;
        Vector2D *tmp = new Vector2D[Nmax];
        memcpy(tmp, tabVertices, (N + 1) * sizeof(Vector2D));
        delete [] tabVertices;
        tabVertices = tmp;
    }
    tabVertices[N] = v;
    N++;
    tabVertices[N] = tabVertices[0];
}

bool SupportPolygon::addVertex(const Vector2D &v) {
    if (N < 3) {
        tabVertices[N] = v;
        N++;
        tabVertices[N] = tabVertices[0];
        return true;
    }
    if (isInside(v)) return false;
    if (N + 1 == Nmax) { //resize
        Nmax += 10;
        Vector2D *tmp = new Vector2D[Nmax];
        memcpy(tmp, tabVertices, (N + 1) * sizeof(Vector2D));
        tabVertices = tmp;
    }
    // search the first edge where P is on the right of the edge
    if (isOnTheLeft(v, 0)) {
        //cout << "cas 1" << endl;
        int i = 1;
        while (i < N && isOnTheLeft(v, i)) {
            i++;
        }
        if (i == N) return false; // normally it must find an edge out
        int j = i + 1;
        while (j < N && !isOnTheLeft(v, j)) {
            j++;
        }
        //cout << "insert between" << i << "and " << j;
        if (j == i + 1) {
            // insert p between P[i] et P[j]
            for (int it = N + 1; it > j; it--) {
                tabVertices[it] = tabVertices[it - 1];
            }
            tabVertices[j] = v;
            N++;
        } else if (j == i + 2) {
            tabVertices[i + 1] = v;
        } else {
            int a = j - i - 2;
            // insert p between P[i] et P[j]
            for (int it = i + 2; it <= N - a; it++) {
                tabVertices[it] = tabVertices[it + a];
            }
            tabVertices[i + 1] = v;
            N -= a;
        }
    } /*else {
        cout << "cas 2" << endl;
    }*/

    // simplify
    Vector2D *p0, *p1, *p2;
    int i = 0;
    bool aligned = false;
    while (i < N && !aligned) {
        p0 = &tabVertices[i];
        p1 = &tabVertices[i + 1];
        p2 = &tabVertices[(i + 2) % N];
        aligned = ((p0->x == p1->x && p0->x == p2->x) ||
                   (p0->y == p1->y && p0->y == p2->y) ||
                   ((p2->x - p0->x != 0) && (p2->y - p0->y != 0) &&
                    (p1->x - p0->x) / (p2->x - p0->x) == (p1->y - p0->y) / (p2->y - p0->y)));
        i++;
    }
    if (aligned) {
        for (int j = i; j < N; j++) {
            tabVertices[j] = tabVertices[j + 1];
        }
        N--;
    }
    return true;
}

// return true if P is on the left of PiPi+1
bool SupportPolygon::isOnTheLeft(const Vector2D &v, int i) {
    Vector2D AB = tabVertices[i + 1] - tabVertices[i];
    Vector2D AP = v - tabVertices[i];
    return (AB.x * AP.y - AB.y * AP.x) >= 0;
}

bool SupportPolygon::isInside(const Vector2D &v) {
    int i = 0;
    while (i < N && isOnTheLeft(v,i)) { i++; }
    return i == N;
}
