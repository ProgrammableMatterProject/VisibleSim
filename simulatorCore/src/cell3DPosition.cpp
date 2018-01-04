#include "cell3DPosition.h"

Cell3DPosition::Cell3DPosition() {
    set(0,0,0);
}

Cell3DPosition::Cell3DPosition(short x,short y,short z) {
    set(x,y,z);
}

void Cell3DPosition::set(short x,short y,short z) {
    pt[0]=x;
    pt[1]=y;
    pt[2]=z;
}

double Cell3DPosition::dist_euclid(const Cell3DPosition p) const {
    Cell3DPosition diff = *this - p;
    return diff.l2_norm();
}

double Cell3DPosition::l2_norm() const {
    return sqrt(pt[0]*pt[0] + pt[1]*pt[1] + pt[2]*pt[2]);
}

ostream& operator<<(ostream& f,const Cell3DPosition&p) {
    f << "(" << p.pt[0] << "," << p.pt[1] << "," << p.pt[2] << ")";
    return f;
}

bool Cell3DPosition::operator<(const Cell3DPosition &o) const {
    if (pt[0] < o.pt[0]) return true;
    else if (pt[0] > o.pt[0]) return false;
    else {
        if (pt[1] < o.pt[1]) return true;
        else if (pt[1] > o.pt[1]) return false;
        else {
            if (pt[2] < o.pt[2]) return true;
            else return false;
        }
    }
}

bool Cell3DPosition::compare_ZYX(const Cell3DPosition& first, const Cell3DPosition& second) {    
    // Consider positions in order z -> y -> x
    if (first.pt[2] < second.pt[2]) return true;
    else if (first.pt[2] > second.pt[2]) return false;
    else {
        if (first.pt[1] < second.pt[1]) return true;
        else if (first.pt[1] > second.pt[1]) return false;
        else {
            if (first.pt[0] < second.pt[0]) return true;
            else return false;
        }
    }
}

const Cell3DPosition operator +(const Cell3DPosition p1, const Cell3DPosition p2)
{
    Cell3DPosition r;
    r.pt[0] = p1.pt[0]+p2.pt[0];
    r.pt[1] = p1.pt[1]+p2.pt[1];
    r.pt[2] = p1.pt[2]+p2.pt[2];
    return r;
}

const Cell3DPosition operator -(const Cell3DPosition p1, const Cell3DPosition p2)
{
    Cell3DPosition r;
    r.pt[0] = p1.pt[0]-p2.pt[0];
    r.pt[1] = p1.pt[1]-p2.pt[1];
    r.pt[2] = p1.pt[2]-p2.pt[2];
    return r;
}

const Cell3DPosition operator *(const Cell3DPosition p1, const Cell3DPosition p2)
{
    Cell3DPosition r;
    r.pt[0] = p1.pt[0]*p2.pt[0];
    r.pt[1] = p1.pt[1]*p2.pt[1];
    r.pt[2] = p1.pt[2]*p2.pt[2];
    return r;
}

const Cell3DPosition operator *(int n, const Cell3DPosition p)
{
    Cell3DPosition r;
    r.pt[0] = n*p.pt[0];
    r.pt[1] = n*p.pt[1];
    r.pt[2] = n*p.pt[2];
    return r;
}
