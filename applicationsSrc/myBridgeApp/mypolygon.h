#ifndef MYPOLYGON_H
#define MYPOLYGON_H

#include "vector2d.h"
#include <vector>
#include <string>
//#include <QPainter>

/*
class Triangle {
public:
    Vector2D *tabPts[3];
    Triangle(Vector2D *p_p0,Vector2D *p_p1,Vector2D *p_p2) {
        tabPts[0]=p_p0;
        tabPts[1]=p_p1;
        tabPts[2]=p_p2;
    }
    bool isOnTheLeft(const Vector2D& P,int i) {
        Vector2D AB=*(tabPts[(i+1)%3])-*(tabPts[i]);
        Vector2D AP=P-*(tabPts[i]);
        return (AB.x*AP.y - AB.y*AP.x)>0;
    }
    bool isInside(const Vector2D& P) {
        return isOnTheLeft(P,0) && isOnTheLeft(P,1) && isOnTheLeft(P,2);
    }
};
*/

class MyPolygon {
public:
    Vector2D *tabPts; // array of vertices (duplicating the first element)
    int Nmax; // maximum number of vertices
    int N; // current number of vertices
    bool selected = false;
    //QVector<Triangle> triangles; // list of triangles for the triangulation

    // return if a point P is on the left of the edge #i
    bool isOnTheLeft(const Vector2D &p, int i) const;

    MyPolygon(int p_Nmax);
    ~MyPolygon();
    bool addPoint(const Vector2D &p);
    bool insertPoint(const Vector2D &p);

    //void draw(QPainter &painter);
    //void triangulate();
    bool isConvex() const;
    bool isOnTheLeft(const Vector2D &P, const Vector2D &A, const Vector2D &B) const;
    bool isInside(const Vector2D &p) const;
    bool getSelected() {return selected;}
    void setSelected(bool value) {selected = value;}
    std::string affichage();
    std::vector<Vector2D> JarvisConvexHull(std::vector<Vector2D>&);
    double DistancePointPolygone(const Vector2D& , const std::vector<Vector2D>&);
};

#endif // MYPOLYGON_H
