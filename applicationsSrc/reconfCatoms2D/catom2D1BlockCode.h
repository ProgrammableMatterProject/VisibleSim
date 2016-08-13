/*
 * catom2D1BlockCode.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef CATOM2D1BLOCKCODE_H_
#define CATOM2D1BLOCKCODE_H_

#include "catoms2DBlockCode.h"
#include "catoms2DSimulator.h"

#include "catoms2DBlock.h"
#include "localTupleSpace.hpp"
#include "tuple.hpp"
#include "contextTuple.hpp"
#include "CTuple.hpp"
//#include "reconfCatoms2DMessages.h"
#include "segment.h"
#include "map.h"
#include "CTuples.h"
#include "landmarks.h"
#include "simulationParameters.h"
#include "reconfiguration.h"

class CTuples;
class Landmarks;

class Catoms2D1BlockCode : public Catoms2D::Catoms2DBlockCode {
 public:
  static SimulationParameters simParams;
  
  Scheduler *scheduler;
  Catoms2D::Catoms2DBlock *catom2D;

  Catoms2D1BlockCode (Catoms2D::Catoms2DBlock *host);
  ~Catoms2D1BlockCode ();

  // Distributed map construction
  Map* map;

  // Geo-routing
  //GPSR gpsr;
  
  //Landmark structure
  Landmarks* landmarks;

  // CTuples system
  CTuples* ctuples;
  
  // Reconfiguration
  Reconfiguration* reconfiguration;

  bool geoTest;
  
  void startup();
  void processLocalEvent(EventPtr pev);
	
  //void updateBorder();
  //bool canMove();

  void setSimulationParameters();
  
  static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* CATOM2DBLOCKCODE_H_ */
