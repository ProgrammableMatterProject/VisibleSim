#include "neighbor.h"

/***********
 * Neighbor Class
 ***********/

Neighbor::Neighbor() {
  interface = NULL;
}

Neighbor::Neighbor(P2PNetworkInterface *i, Coordinate &p) {
  interface = i;
  position = p;
}

Neighbor::Neighbor(const Neighbor &n) {
  interface = n.interface;
  position = n.position;
}

Neighbor::~Neighbor() {}

string Neighbor::toString() {
  string s = "Neighbor: " + position.toString();
  return s;
}
