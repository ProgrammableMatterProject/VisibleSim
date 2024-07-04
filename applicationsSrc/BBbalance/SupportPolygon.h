//
// Created by bpiranda on 19/10/2023.
//

#ifndef VISIBLESIM_SUPPORTPOLYGON_H
#define VISIBLESIM_SUPPORTPOLYGON_H
#include <vector>

class Vector2D {
public:
    float x,y;
    Vector2D(float p_x,float p_y):x(p_x),y(p_y) {};
    Vector2D():x(0),y(0) {};
    Vector2D operator+(const Vector2D &op) const {
        return Vector2D(x+op.x,y+op.y);
    }
    Vector2D operator-(const Vector2D &op) const {
        return Vector2D(x-op.x,y-op.y);
    }
};

class SupportPolygon {
public :
    Vector2D *tabVertices= nullptr;
    int Nmax;
    int N;
    SupportPolygon() { Nmax=5; N=0; tabVertices=new Vector2D[Nmax]; }
    SupportPolygon(int p_Nmax):Nmax(p_Nmax) { N=0; tabVertices=new Vector2D[Nmax]; }
    SupportPolygon(const SupportPolygon& src);
    ~SupportPolygon() { delete [] tabVertices; }
    bool addVertex(const Vector2D &v);
    void addLastVertex(const Vector2D &v);
    bool isInside(const Vector2D &v);
    bool isOnTheLeft(const Vector2D& P,int i);
    void reset() { N=0; }
};


#endif //VISIBLESIM_SUPPORTPOLYGON_H
