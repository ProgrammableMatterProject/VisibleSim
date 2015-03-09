#include "cell3DPosition.h"

Cell3DPosition::Cell3DPosition() {
    set(0,0,0);
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

