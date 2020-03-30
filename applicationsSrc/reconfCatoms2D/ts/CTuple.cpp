#include "CTuple.hpp"

using namespace std;

Rectangle CTuple::bounds;

CTuple::CTuple(ContextTuple t): position(t.getPosition()), tuple(t) {};
 
CTuple::CTuple(Tuple t): tuple(t) {
    size_t hash = t.hash();
    Coordinate p1 = bounds.getPBottomLeft();
    Coordinate p2 = bounds.getPTopRight();
    position.x = p1.x + hash%(p2.x-p1.x);
    position.y = p1.y + hash%(p2.y-p1.y);
};
 
CTuple::CTuple(const CTuple& c): position(c.position), tuple(c.tuple) {};

CTuple::~CTuple() {}

Tuple& CTuple::getTuple() {return tuple;};
Coordinate CTuple::getPosition() const {return position;};

bool CTuple::match(CTuple &t) const {
  return tuple.match(t.getTuple());
}

void CTuple::setBounds(Rectangle &r) {
  bounds = r;
}

std::ostream& operator<<(std::ostream& os, const CTuple &ct) {
  os << "position: " << ct.position << " :";
  os << ct.tuple;
  return os;
}
