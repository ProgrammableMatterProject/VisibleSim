#pragma once
#include <cstdlib>
#include <cmath>
#define EPS 1E-150
#define EPS_BORDER 1E-15

class Point
{
public:
    double x;
    double y;
    double z;
    Point (double _x, double _y, double _z) : x(_x), y(_y), z(_z) {};
    Point() {};

    void print_point() const;
    double dist(const Point&) const;
    bool are_collinear(const Point&, const Point&) const;

    bool operator==(const Point &);
    double operator *(Point &);
    Point operator-(const Point &) const;
    Point operator ^(const Point &);
};

class Equation
{
public:
    double a;
    double b;
    double c;
    double d;
};
