#include <cstdio>
#include "triangle.h"
#include "world.h"

Triangle::Triangle(int p1_idx, int p2_idx, int p3_idx)
{
    p1 = MeshWorld::points[p1_idx];
    p2 = MeshWorld::points[p2_idx];
    p3 = MeshWorld::points[p3_idx];
    get_normal();
}

void
Triangle::print_points()
{
    p1->print_point();
    p2->print_point();
    p3->print_point();
}

/* Assume the point is inside the polygon */
bool
Triangle::is_in_border(const Point &p)
{
    if (p.are_collinear(*p1, *p2) || p.are_collinear(*p1, *p3) || p.are_collinear(*p2, *p3))
        return true;
    return false;
}

bool
Triangle::is_point_in_polygon(const Point &p)
{
    Point v0 = *p3 - *p1;
    Point v1 = *p2 - *p1;
    Point v2 = p - *p1;

    double dot00 = v0*v0;
    double dot01 = v0*v1;
    double dot02 = v0*v2;
    double dot11 = v1*v1;
    double dot12 = v1*v2;

    double invDenom = 1/(dot00 * dot11 - dot01 * dot01);
    double u = (dot11*dot02 - dot01*dot12)*invDenom;
    double v = (dot00*dot12 - dot01*dot02)*invDenom;
    return (u >= 0) && (v >= 0) && (u + v < 1);
}
