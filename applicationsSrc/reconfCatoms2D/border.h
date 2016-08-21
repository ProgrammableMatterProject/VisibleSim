#ifndef BORDER_H_
#define BORDER_H_

#include "catoms2DBlock.h"
#include "network.h"
#include "coordinate.h"

class Border {
 private:
  Catoms2D::Catoms2DBlock* catom;
  
 public:
  Border(Catoms2D::Catoms2DBlock *c); 
  Border(const Border &b);
  ~Border();

  //P2PNetworkInterface* getCCWInterface();
  //P2PNetworkInterface* getCWInterface();
  P2PNetworkInterface* getInterface(Catoms2D::RelativeDirection::Direction d);
};

#endif
