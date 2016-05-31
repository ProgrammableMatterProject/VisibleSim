/*
 * neighbor_finding_BlockCode.h
 *
 *  Created on: 21 avril 2013
 *      Author: nico
 */

#ifndef coloration_v2_BlockCode_H_
#define coloration_v2_BlockCode_H_

#include "smartBlocksBlockCode.h"
#include "smartBlocksSimulator.h"
#include "smartBlocksScheduler.h"
#include "smartBlocksBlock.h"
#include <map>
#include <boost/array.hpp>
#include "neighbor_finding.h"
#include "coloring.h"

using namespace std;
using namespace neighbor_finding;
using namespace coloring;

class ColorationV2BlockCode : public SmartBlocks::SmartBlocksBlockCode {
  // NeighborsFinding //
  void TreatRouteSearchMsg( RouteSearchMsgPtr);
  void TreatRouteSearchAck( RouteSearchAckPtr);
  list<BlockAckInfos> BuildDistancesList( list<Coordinates>);
  void StartNextRound();
  void ContinueWhenNeighborsAreFound();

  std::map<Coordinates, BlockRoutingInfos> routes_map_;
  // This says if one of my RouteSearchMsg reached only dead-ends by neighbor[ i]
  bool dead_end_neighbors_[ cNeighborQuantity];
  unsigned int n_round_;
  bool neighbor_finding_done_;
  //
  // Coloration //
  void TreatColoringMsg( ColoringMsgPtr);
  void TreatColoringAck( ColoringAckPtr);
  //~ void PickColor();
  void DrawAndSendColorScores();
  void ProcessScores();

  map<Coordinates, ColorQuantityArray> color_neighborhood_;
  ColorQuantityArray my_scores_;
  list<int> allowed_colors_;
  unsigned int n_color_msg_received_;
  int my_color_; // optional but may be convenient for other features
  //
  // Graphics //
  //~ unsigned int display_color_;
  //~ void color();

  //DEBUG
  void PrintRoutesMap();
public:
  SmartBlocks::SmartBlocksScheduler *scheduler;
  SmartBlocks::SmartBlocksBlock *smartBlock;

  ColorationV2BlockCode( SmartBlocks::SmartBlocksBlock *host);
  ~ColorationV2BlockCode();

  void startup();
  void processLocalEvent( EventPtr pev);

  static SmartBlocks::SmartBlocksBlockCode *buildNewBlockCode( SmartBlocks::SmartBlocksBlock *host);
};
#endif /* coloration_v2_BlockCode_H_ */
