#include <cstdio>
#include "polygon.h"

void Polygon::get_normal()
{
    Point vector1; 
    vector1.x = p2->x - p1->x;
    vector1.y = p2->y - p1->y;
    vector1.z = p2->z - p1->z;

    Point vector2; 
    vector2.x = p3->x - p1->x;
    vector2.y = p3->y - p1->y;
    vector2.z = p3->z - p1->z;

    normal.a = vector1.y*vector2.z - vector1.z*vector2.y;
    normal.b = vector1.z*vector2.x - vector1.x*vector2.z;
    normal.c = vector1.x*vector2.y - vector1.y*vector2.x;

    normal.d = -( normal.a*p1->x + normal.b*p1->y + normal.c*p1->z);
}

void Polygon::print_normal()
{
    printf("%lfx + %lfy + %lfz = %lf\n", normal.a, normal.b, normal.c, normal.d); 
}

/*
 * V(1, 0.5, 0.7)
 * Ray P = p + tV
 * Plane  P*N + d = 0
 * t = -(p*N + d)/(V*N);
 * Should T >= 0 ? Ray just in one direction.
 * Return:
 *  0 if ray does not intersect.
 *  1 if ray intersect on the borders.
 *  2 if ray intersect.
 */
int Polygon::ray_intersect(const Point &p, const Point &v)
{
    double pnormal = p.x*normal.a + p.y*normal.b + p.z*normal.c;
    // Right angle
    if (!(normal.a*v.x + normal.b*v.y + normal.c*v.z)) {
        return 0;
    }

    double t = -(pnormal + normal.d) / (normal.a*v.x + normal.b*v.y + normal.c*v.z);

    if (t < 0)
        return 0;

    Point _p1(p.x + t*v.x, p.y + t*v.y, p.z + t*v.z);

    if (is_point_in_polygon(_p1)) {
        if (is_in_border(_p1)) {
            return 1;
        }
        return 2;
    }
    return 0;
}

/* Check if point is on the same plane and then check the polygon */
bool Polygon::contains_point(const Point &p)
{
    if (fabs(normal.a*p.x + normal.b*p.y + normal.c*p.z + normal.d) < EPS_BORDER)
    {
       return is_point_in_polygon(p);
    }
    return false;
}

Point Polygon::get_intersection_point(const Point &p, const Point &v)
{
    double pnormal = p.x*normal.a + p.y*normal.b + p.z*normal.c;

    double t = -(pnormal + normal.d) / (normal.a*v.x + normal.b*v.y + normal.c*v.z);

    return Point(p.x + t*v.x, p.y + t*v.y, p.z + t*v.z);
}
