#pragma once
#include "polygon.h"

class Triangle : public Polygon
{

public:
    Triangle(int _p1_idx, int _p2_idx, int _p3_idx);

    void print_points();
    bool is_in_border(const Point &);
    bool is_point_in_polygon(const Point &);
};
