/*!
 * \file cell3DPosition.h
 * \brief 3D integer position in a grid
 * \date 29/01/2012
 * \author Benoît Piranda
 */

#ifndef CELL3DPOSITION_H
#define CELL3DPOSITION_H

#include <iostream>
#include <fstream>
#include <cmath>
#include "vector3D.h"

using namespace std;

class Cell3DPosition {
public:
    short pt[3]; //!< (x,y,z) values of the vector
    Cell3DPosition();
    Cell3DPosition(short x,short y,short z);

    void set(short x,short y,short z);
    Cell3DPosition addX(short x) const;
    Cell3DPosition addY(short y) const;
    Cell3DPosition addZ(short z) const;

    inline const short operator[](const int i) const { return pt[i]; };
    bool operator<(const Cell3DPosition &o) const;
    bool operator==(const Cell3DPosition &o) const
        { return (pt[0] == o.pt[0]) && (pt[1] == o.pt[1]) && (pt[2] == o.pt[2]) ; };
    operator Vector3D() const { return Vector3D(pt[0], pt[1], pt[2], 1.0); };
    
    friend ostream& operator<<(ostream& f,const Cell3DPosition&p);
    friend const Cell3DPosition operator +(const Cell3DPosition,const Cell3DPosition);
    friend const Cell3DPosition operator -(const Cell3DPosition,const Cell3DPosition);
    friend const Cell3DPosition operator *(const Cell3DPosition,const Cell3DPosition);
};

#endif // CELL3DPOSITION_H
