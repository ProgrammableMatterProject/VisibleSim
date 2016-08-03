#include "border.h"

using namespace std;
using namespace Catoms2D;

Border::Border(Catoms2DBlock *c) {
  catom = c;
}

Border::Border(const Border &b) {
  catom = b.catom;
}

Border::~Border(){}

P2PNetworkInterface* Border::getInterface(Catoms2D::RelativeDirection::Direction d) {
  P2PNetworkInterface *p1 = NULL, *p2 = NULL;
  Catoms2D::RelativeDirection::Direction od =
    Catoms2D::RelativeDirection::getOpposite(d);
  
  if (catom->nbNeighbors(true) == 0) {
    return NULL;
  }

  int cn = catom->nbConsecutiveNeighbors(true);
  
  // pick-up a neighbor of c in the longest sequence of consecutive neighbors
  for (int i = 0; i < HLattice::Direction::MAX_NB_NEIGHBORS; i++) {
    int n = 0;
    
    p1 = catom->getInterface(i);
    if (!catom->hasANeighbor(p1,false)) {
      continue;
    }
    n = 1;
    p2 = p1;
    
    while (n != cn) {
      p2 = catom->getNextInterface(od,p2);
      if(!catom->hasANeighbor(p2,true)) {
	break;
      }
      n++;
    }
    if (n == cn) {
      break;
    }
  }

  if (catom->nbNeighbors(true) == 1) {
    return p1;
  }
  
  p2 = p1;
  while (true) {
    p2 = catom->getNextInterface(od,p2);
    if (!catom->hasANeighbor(p2,true)) {
      return p1;
    }
    if (catom->hasANeighbor(p2,false)) {
      p1 = p2;
    }
  }
  return NULL;
}
