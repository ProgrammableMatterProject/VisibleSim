#ifndef SQUELETON_H
#define SQUELETON_H

#include <trace.h>
#include <vector>
#include <vector3D.h>

class Skeleton {

public :
    Skeleton() {};
    Skeleton(double r,double b):radius2(r*r),blobbiness(b) {};
    virtual ~Skeleton();

    virtual void add(Skeleton *);
    double potentiel(const Vector3D &pos);
protected :
    virtual double distance2(const Vector3D &pos) { return -1.0; };
    double radius2,blobbiness;
    vector <Skeleton*> children;
};

class SkelLine : public Skeleton {
public :
    SkelLine(const Vector3D &A,const Vector3D &B,double r,double b):Skeleton(r,b),ptA(A),ptB(B) { AB=B-A; AB2 = AB.norme2(); };
    ~SkelLine() {};
protected :
    Vector3D ptA,ptB,AB;
    double AB2;
    virtual double distance2(const Vector3D &pos);
};

class SkelPoint : public Skeleton {

public :
    SkelPoint(const Vector3D &pt,double r,double b):Skeleton(r,b),ptA(pt) {};
    ~SkelPoint() {};
protected :
    Vector3D ptA;
    virtual double distance2(const Vector3D &pos);
};

#endif // SQUELETON_H
