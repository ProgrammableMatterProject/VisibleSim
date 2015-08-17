#ifndef SQUELETON_H
#define SQUELETON_H

#include <trace.h>
#include <vector>
#include <vecteur.h>

class Skeleton {

public :
    Skeleton() {};
    Skeleton(double r,double b):radius2(r*r),blobbiness(b) {};
    virtual ~Skeleton();

    virtual void add(Skeleton *);
    double potentiel(const Vecteur &pos);
protected :
    virtual double distance2(const Vecteur &pos) { return -1.0; };
    double radius2,blobbiness;
    vector <Skeleton*> children;
};

class SkelLine : public Skeleton {
public :
    SkelLine(const Vecteur &A,const Vecteur &B,double r,double b):Skeleton(r,b),ptA(A),ptB(B) { AB=B-A; AB2 = AB.norme2(); };
    ~SkelLine() {};
protected :
    Vecteur ptA,ptB,AB;
    double AB2;
    virtual double distance2(const Vecteur &pos);
};

class SkelPoint : public Skeleton {

public :
    SkelPoint(const Vecteur &pt,double r,double b):Skeleton(r,b),ptA(pt) {};
    ~SkelPoint() {};
protected :
    Vecteur ptA;
    virtual double distance2(const Vecteur &pos);
};

#endif // SQUELETON_H
