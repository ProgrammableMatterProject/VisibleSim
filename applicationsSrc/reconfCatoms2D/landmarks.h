#ifndef LANDMARKS_H_
#define LANDMARKS_H_

#include <vector>
#include "coordinate.h"
//#include "time.h"
#include "network.h"
#include "events.h"
#include <boost/shared_ptr.hpp>

#define Time uint64_t

//typedef landmark_address vector<int>;

// Record of a landmark in the landmark table
class Landmark {
 public:
  int id;
  int distance;
  Coordinate position;
  Time time;
    
  Landmark();
  Landmark(int i, int d, Coordinate p, Time t);
  Landmark(const Landmark &l);
  ~Landmark();

  void update(int i, int d, Coordinate p, Time t);
  
 protected:
  void set(int i, int d, Coordinate p, Time t);
};

class Landmarks {
 public:
  vector<Landmark> landmarks;

  Landmarks();
  ~Landmarks();

  bool handleMessage(MessagePtr message);
  void handleEvent(EventPtr pev);
};

// Events
#define LANDMARK_BEACON_PERIOD_SEC 20 
#define LANDMARK_BEACON_EVENT 16000

/*
class TryToMoveEvent : public BlockEvent {
 public:

 TryToMoveEvent(uint64_t t, BaseSimulator::BuildingBlock *conBlock): BlockEvent(t, conBlock) {
    eventType = EVENT_TRY_TO_MOVE;
    randomNumber = conBlock->getNextRandomNumber();
  }
	
 TryToMoveEvent(TryToMoveEvent *ev) : BlockEvent(ev) {
    randomNumber = ev->randomNumber;
  }
	
  ~TryToMoveEvent() {};
	
  void consumeBlockEvent() {
    concernedBlock->scheduleLocalEvent(EventPtr(new TryToMoveEvent(this)));
  }
	
  const string getEventName() { return "TRY_TO_MOVE EVENT"; }
};
*/

// Messages
#define LANDMARK_BEACON_MSG 16000

class LandmarkBeaconMessage;
typedef boost::shared_ptr<LandmarkBeaconMessage> LandmarkBeaconMessage_ptr;

class LandmarkBeaconMessage : public Message {
  
 public:
  int id;
  Coordinate position;
  int distance;
  
 LandmarkBeaconMessage(int i, Coordinate p, int d) : Message() { 
    type = LANDMARK_BEACON_MSG;
    id = i;
    position = p;
    distance = d;
  };

 LandmarkBeaconMessage(LandmarkBeaconMessage *m) : Message() { 
    type = m->type;
    id = m->id;
    position = m->position;
    distance = m->distance;
  };
  
  ~LandmarkBeaconMessage() {};
  unsigned int size() { return 17;};
};

#endif
