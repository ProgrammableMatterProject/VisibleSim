#include <iostream>
#include "centralizedReconfiguration.h"

using namespace std;

void centralized_reconfiguration() {
  cout << "centralized reconfiguration" << endl;
  Catoms2DWorld *world = Catoms2DWorld::getWorld();
  
  /* Current map:
     int *gridSize = world->getGridSize();
     for (int iy = 0; iy < gridSize[2]; iy++) {
     for (int ix = 0; ix < gridSize[0]; ix++) {
     Catoms2DBlock* c = world->getGridPtr(ix,0,iy);
     if (c != NULL) {
	
     }
     }
     }*/

  /* Target map:
  Coordinate pmin(INT_MAX,INT_MAX);
  Coordinate pmax(INT_MIN,INT_MIN);
  Catoms2DWorld *world = Catoms2DWorld::getWorld();
  int *gridSize = world->getGridSize();
  for (int iy = 0; iy < gridSize[2]; iy++) {
    for (int ix = 0; ix < gridSize[0]; ix++) {
      if (world->getTargetGrid(ix,0,iy) == fullCell ) {
	pmin.x = min(pmin.x, ix);
	pmin.y = min(pmin.y, iy);
	pmax.x = max(pmax.x, ix);
	pmax.y = max(pmax.y, iy);
	Coordinate real(ix,iy);
	Coordinate t =  map.real2Virtual(real);
	cout << "target: (" << t.x << " " << t.y << ")" << endl;
	ctuples.out(ContextTuple(string("target"),t));
      }
    }
  }
  Rectangle bounds(pmin,pmax);
  CTuple::setBounds(bounds);
  */
}
