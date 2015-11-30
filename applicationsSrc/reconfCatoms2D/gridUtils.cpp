#include "catoms2DWorld.h"
#include "gridUtils.h"

using namespace std;
using namespace Catoms2D;

bool isNeighbor(Catoms2DBlock *c,Coordinate &p) {  
  // test all neighbors cell
  for (int i = 0; i < 6; i++) {
    Vecteur v = c->getPosition((NeighborDirection::Direction)i);
    Coordinate ci(v[0],v[2]);
    if (ci == p) {
      return true;
    }
  }
  return false;
}

bool isInTarget(Coordinate &p) { 
  Catoms2DWorld *world = Catoms2DWorld::getWorld();
  return (world->getTargetGrid(p.x,0,p.y) == fullCell);
}

/*Coordinate getPosition(Catoms2DBlock* c, Catoms2DMove &m) {
   P2PNetworkInterface *p2p = m.getPivot()->getP2PNetworkInterfaceByBlockRef(c);
   Coordinate position(m.getPivot()->position[0], m.getPivot()->position[2]);
   p2p = nextInterface(m.getPivot(),m.getDirection(),p2p);
   return Map::getPosition(m.getPivot(),position,p2p);
}*/

