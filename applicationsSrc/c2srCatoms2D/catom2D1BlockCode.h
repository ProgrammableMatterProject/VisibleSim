/*
 * catom2D1BlockCode.h
 *
 *  Created on: 25 Nov 2015
 *      Author: Andre Naz
 */

#ifndef CATOM2D1BLOCKCODE_H_
#define CATOM2D1BLOCKCODE_H_

#include "catoms2DBlock.h"
#include "catoms2DBlockCode.h"
#include "catoms2DSimulator.h"

#include "simulationParameters.h"
#include "map.h"
#include "c2sr.h"

class Catoms2D1BlockCode : public Catoms2D::Catoms2DBlockCode {
 public:
  static SimulationParameters simParams;
  
  Scheduler *scheduler;
  Catoms2D::Catoms2DBlock *catom2D;

  Catoms2D1BlockCode (Catoms2D::Catoms2DBlock *host);
  ~Catoms2D1BlockCode ();

  // Distributed map construction
  Map* map;
  
  // C2SR
  C2SR* c2sr;
  
  void startup();
  void processLocalEvent(EventPtr pev);
  
  void setSimulationParameters();
  void setCommunicationRate();
  void setMotionSpeed();

  void scheduleC2SRStart();
  
  static BlockCode *buildNewBlockCode(BuildingBlock *host);
};

#endif /* CATOM2DBLOCKCODE_H_ */
