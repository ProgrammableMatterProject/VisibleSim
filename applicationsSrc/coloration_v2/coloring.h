#ifndef COLORING_H_
#define COLORING_H_

#include <list>
#include <map>
#include <boost/shared_ptr.hpp>
#include "network.h"
#include "coordinates.h"
#include <boost/array.hpp>
#include "smartBlocksBlock.h"

namespace coloring {

//~ using namespace SmartBlocks;

class ColoringMsg;
class ColoringAck;

typedef boost::shared_ptr<ColoringMsg> ColoringMsgPtr;
typedef boost::shared_ptr<ColoringAck> ColoringAckPtr;

const int cNeighborsQuantity( 8);
const int cColorQuantity( 10);

typedef boost::array<int, cColorQuantity> ColorQuantityArray;

const int cColoringMsgId( 3);
const int cColoringAckId( 4);

//~ struct ColorationInfo {
  //~ int scores[ cColorQuantity];
//~ };

class ColoringMsg : public Message {
  ColorQuantityArray scores_;
  Coordinates origine_;
  Coordinates destination_;
public:
  ColoringMsg( Coordinates, Coordinates, ColorQuantityArray, NeighborDirection);
  ColoringMsg( const ColoringMsgPtr, NeighborDirection);
  ~ColoringMsg() {}

  ColorQuantityArray scores() { return scores_; };
  Coordinates origine() { return origine_; }
  Coordinates destination() { return destination_; }

  void print();
};

class ColoringAck : public Message {
  Coordinates origine_;
  Coordinates destination_;
  int color_;
public:
  ColoringAck( Coordinates, Coordinates, int, NeighborDirection);
  ColoringAck( const ColoringAckPtr, NeighborDirection);
  ~ColoringAck() {}

  Coordinates origine() { return origine_; };
  Coordinates destination() { return destination_; }
  int color() { return color_; }

  void print();
};
}
#endif // COLORING_H_
