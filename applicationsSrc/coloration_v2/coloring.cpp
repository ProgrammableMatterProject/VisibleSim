#include "coloring.h"

using namespace coloring;

ColoringMsg::ColoringMsg( Coordinates origine, Coordinates destination,
                          ColorQuantityArray scores,
                          NeighborDirection direction)
    : scores_( scores), origine_( origine), destination_( destination) {
  type = cColoringMsgId;
  origine_.AdaptToBeSentBy( direction);
  destination_.AdaptToBeSentBy( direction);
print();
}

ColoringMsg::ColoringMsg( const ColoringMsgPtr msg, NeighborDirection direction)
    : scores_( msg->scores_), origine_( msg->origine_), destination_( msg->destination_) {
  type = cColoringMsgId;
  origine_.AdaptToBeSentBy( direction);
  destination_.AdaptToBeSentBy( direction);
print();
}

ColoringAck::ColoringAck( Coordinates origine, Coordinates destination,
                          int color,
                          NeighborDirection direction)
    : origine_( origine), destination_( destination), color_( color) {
  type = cColoringAckId;
  origine_.AdaptToBeSentBy( direction);
  destination_.AdaptToBeSentBy( direction);
print();
}

ColoringAck::ColoringAck( const ColoringAckPtr ack, NeighborDirection direction)
    : origine_( ack->origine_), destination_( ack->destination_),
      color_( ack->color_) {
  type = cColoringAckId;
  origine_.AdaptToBeSentBy( direction);
  destination_.AdaptToBeSentBy( direction);
print();
}



void ColoringMsg::print() {
  cout << "ColorMsg:" << endl;
  cout << "Origine = " << origine_.x << ", " << origine_.y << endl;
  cout << "Destination = " << destination_.x << ", " << destination_.y << endl;
  cout << "Scores : ";

  for( int aa = 0; aa < cColorQuantity; ++aa) {
    cout << scores_[ aa] << " ";
  }
  cout << endl << endl;
}

void ColoringAck::print() {
  cout << "ColorAck:" << endl;
  cout << "Origine = " << origine_.x << ", " << origine_.y << endl;
  cout << "Destination = " << destination_.x << ", " << destination_.y << endl;
  cout << "Color = " << color_ << endl << endl;
}
