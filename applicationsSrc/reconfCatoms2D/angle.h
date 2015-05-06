#ifndef ANGLE_H_
#define ANGLE_H_

#include <math.h>
#include "coordinate.hpp"

#define PI 3.14159265
// counterwise clock angle abc
double ccwAngle(Coordinate &a, Coordinate &b, Coordinate &c) {
  Coordinate v1;
  Coordinate v2;
  
  v1.x = a.x - b.x;
  v1.y = a.y - b.y;
  
  v2.x = c.x - b.x;
  v2.y = c.y - b.y;

  int dot = v1.x*v2.x + v1.y*v2.y;      // dot product
  int det = v1.x*v2.y - v1.y*v2.x;      // determinant
  double angle = atan2(det, dot) * 180 / PI;

  if (det < 0) {
    return -angle;
  }
  else {
    return 360-angle;
  }
}

#endif
