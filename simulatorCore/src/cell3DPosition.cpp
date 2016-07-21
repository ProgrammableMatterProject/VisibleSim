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
            if (pt[0] < o.pt[0]) return true;
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
