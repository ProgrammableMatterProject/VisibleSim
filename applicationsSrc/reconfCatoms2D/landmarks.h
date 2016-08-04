#ifndef LANDMARKS_H_
#define LANDMARKS_H_

#include <vector>
#include <random>

#include "coordinate.h"
//#include "time.h"
#include "network.h"
#include "events.h"
#include <memory>

#include "catom2D1BlockCode.h"

#define Time uint64_t

//typedef landmark_address vector<int>;

class Catoms2D1BlockCode;

class Landmark {
 protected:
  static double probability;
  std::ranlux48 generator;
  std::uniform_real_distribution<> dis;

 public:
  bool isLandmark;

  Landmark();
  Landmark(int seed);
  Landmark(const Landmark &l);
  ~Landmark();
  
  static void setProbability(double p);
  void randomDraw();
};

// Record of a landmark in the landmark table
class LandmarkEntry {
 public:
  int id;
  int distance;
  Coordinate position;
  Time time;
    
  LandmarkEntry();
  LandmarkEntry(int i, int d, Coordinate p, Time t);
  LandmarkEntry(const LandmarkEntry &l);
  ~LandmarkEntry();

  void update(int i, int d, Coordinate p, Time t);
  
 protected:
  void set(int i, int d, Coordinate p, Time t);
};

class Landmarks {
 public:
  bool mustStop;
  Landmark *landmark;
  vector<LandmarkEntry> landmarks;

  Catoms2D1BlockCode *blockCode; // access to map, host, ... 

  Landmarks(Catoms2D1BlockCode *bc);
  Landmarks(const Landmarks &l);
  ~Landmarks();

  void start();
  bool handle(MessagePtr message);
  void scheduleAdvertise(Time t);
  void advertise(LandmarkEntry &l, P2PNetworkInterface *ignore);
  bool update(LandmarkEntry &l);
  void handle(EventPtr pev);
  void stop();

  void printTable();
};

// Events
#define LANDMARK_BEACON_PERIOD_SEC 20
#define SEC_TO_MS(t) (t*1000)
#define LANDMARK_BEACON_EVENT 16000

class LandmarkBeaconEvent : public BlockEvent {
 public:

 LandmarkBeaconEvent(uint64_t t, BaseSimulator::BuildingBlock *conBlock): BlockEvent(t, conBlock) {
    eventType = LANDMARK_BEACON_EVENT;
    randomNumber = conBlock->getNextRandomNumber();
  }
	
 LandmarkBeaconEvent(LandmarkBeaconEvent *ev) : BlockEvent(ev) {
    randomNumber = ev->randomNumber;
  }
	
  ~LandmarkBeaconEvent() {};
	
  void consumeBlockEvent() {
    concernedBlock->scheduleLocalEvent(EventPtr(new LandmarkBeaconEvent(this)));
  }
	
  const string getEventName() { return "LANDMARK_BEACON_EVENT"; }
};

// Messages
#define LANDMARK_BEACON_MSG 16000

class LandmarkBeaconMessage;
typedef std::shared_ptr<LandmarkBeaconMessage> LandmarkBeaconMessage_ptr;

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
