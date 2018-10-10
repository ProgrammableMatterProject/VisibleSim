#pragma once
#include "geometrics.h"

class Polygon
{
protected:
    Equation normal;

    Point *p1;
    Point *p2;
    Point *p3;

public:
    void get_normal();
    void print_normal();

    virtual void print_points() = 0;

    int ray_intersect(const Point &, const Point &);
    bool contains_point(const Point &);
    Point get_intersection_point(const Point &p, const Point &vector);

    virtual bool is_in_border(const Point &) = 0;
    virtual bool is_point_in_polygon(const Point &) = 0;
};
