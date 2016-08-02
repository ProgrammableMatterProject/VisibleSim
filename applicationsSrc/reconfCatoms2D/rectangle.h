#ifndef RECTANGLE_H_
#define RECTANGLE_H_

class Rectangle {
 protected:
  Coordinate pBottomLeft;
  Coordinate pTopRight;
 public:
  
  Rectangle() {}

  Rectangle(Coordinate p1, Coordinate p2) {
    pBottomLeft = p1;
    pTopRight = p2;
  }
  
  Rectangle(Rectangle const &r) {
    pBottomLeft = r.pBottomLeft;
    pTopRight = r.pTopRight;
  }
  
  ~Rectangle() {}

  Coordinate getPBottomLeft() const {
    return pBottomLeft;
  }
  
  Coordinate getPTopRight() const {
    return pTopRight;
  }
  
  Coordinate getPBottomRight() const {
    return Coordinate(pTopRight.x,pBottomLeft.y);
  }

  Coordinate getPTopLeft() const {
    return Coordinate(pBottomLeft.x,pTopRight.y);
  }

  int getWidth() const {
    return (pTopRight.x - pBottomLeft.x) + 1;
  }

  int getHeight() const {
    return (pTopRight.y - pBottomLeft.y) + 1;
  }

  bool contains(Coordinate p) const {
    return (p.getX() >= pBottomLeft.getX()) &&
      (p.getX() <= pTopRight.getX()) &&
      (p.getY() >= pBottomLeft.getY()) &&
      (p.getY() <= pTopRight.getY());
  }
};

#endif
