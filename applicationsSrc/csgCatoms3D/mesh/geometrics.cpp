#include <cstdio>
#include "geometrics.h"

bool Point::operator==(const Point &p) {
    return ((fabs(p.x - x) < EPS) && (fabs(p.y - y) < EPS) && (fabs(p.z - z) < EPS));
}

Point Point::operator-(const Point &p) const {
    return Point(x-p.x, y-p.y, z-p.z);
}

Point Point::operator ^(const Point &v)
{
    Point v1;
    v1.x = y*v.z - z*v.y;
    v1.y = z*v.x - x*v.z;
    v1.z = x*v.y - y*v.x;
    return v1;
}

double Point::operator *(Point &v)
{
    return x*v.x + y*v.y + z*v.z;
}

void Point::print_point() const
{
    printf("(%lf, %lf, %lf)\n", x, y, z);
}

double Point::dist(const Point &p) const {
    return sqrt((x-p.x)*(x-p.x) + (y-p.y)*(y-p.y) + (z-p.z)*(z-p.z));
}

bool
Point::are_collinear(const Point &p1, const Point &p2) const
{
    Point v1 = p1 - *this;
    Point v2 = p2 - *this;
    Point vr = v1 ^ v2;
    if (fabs(vr.x) < EPS_BORDER && fabs(vr.y) < EPS_BORDER && fabs(vr.z) < EPS_BORDER)
        return true;
    return false;
}

