#include "landmarks.h"

//#define LANDMARKS_DEBUG

/**** Landmark Class ****/
#define PROBABILITY_LANDMARK 0.1

double Landmark::probability = PROBABILITY_LANDMARK;

Landmark::Landmark() {
  generator = boost::rand48(0);
  isLandmark = false;
}

Landmark::Landmark(int seed) {
  generator = boost::rand48(seed);
  isLandmark = false;
}

Landmark::Landmark(const Landmark &l) {
  isLandmark = l.isLandmark;
  generator = l.generator;
}

Landmark::~Landmark() {}
  
void Landmark::setProbability(double p) {
  probability = p;
}

void Landmark::randomDraw() {
  double p = ((double) generator() / ((double)boost::rand48:: max()));
  if (p<=probability) {
    isLandmark = true;
  } else {
    isLandmark = false;
  }
}

/**** LandmarkEntry Class ****/

LandmarkEntry::LandmarkEntry() {
  set(0,0,Coordinate(),0);
}

LandmarkEntry::LandmarkEntry(int i, int d, Coordinate p, Time t) {
  set(i, d, p, t);
}

LandmarkEntry::LandmarkEntry(const LandmarkEntry &l) {
  set(l.id, l.distance, l.position, l.time);
}

LandmarkEntry::~LandmarkEntry() {}

void LandmarkEntry::update(int i, int d, Coordinate p, Time t) {
  set(i, d, p, t);
}

void LandmarkEntry::set(int i, int d, Coordinate p, Time t) {
  id = i;
  distance = d;
  position = p;
  time = t;
}

/**** Landmarks Class ****/
#define MAX_NUM_LANDMARKS 10

Landmarks::Landmarks(Catoms2D1BlockCode *bc) {
  blockCode = bc;
  landmark = new Landmark(bc->hostBlock->blockId);
  landmark->randomDraw();
  mustStop = false;

  if (landmark->isLandmark) {
#ifdef LANDMARKS_DEBUG
    cout << bc->hostBlock->blockId << " is landmark!" << endl;
#endif
    BaseSimulator::Scheduler *scheduler =  BaseSimulator::getScheduler();
    scheduleAdvertise(scheduler->now());
  }
}

Landmarks::Landmarks(const Landmarks &l) {
  landmark = l.landmark;
  landmarks = l.landmarks;
  mustStop = l.mustStop;
}

Landmarks::~Landmarks() {
  delete landmark;
}

void Landmarks::scheduleAdvertise(Time t) {
  BaseSimulator::Scheduler *scheduler =  BaseSimulator::getScheduler();
  scheduler->schedule(new LandmarkBeaconEvent(t,blockCode->hostBlock));
}

void Landmarks::advertise(LandmarkEntry &l, P2PNetworkInterface *ignore) {
  list<P2PNetworkInterface*> &p2pList = blockCode->hostBlock->getP2PNetworkInterfaceList();
  list<P2PNetworkInterface*>::iterator it;
  P2PNetworkInterface *p2p;
  for (it = p2pList.begin(); it != p2pList.end(); it++) {
    p2p = *it;
    if( (p2p == ignore) || !p2p->connectedInterface) {
      continue;
    }
    LandmarkBeaconMessage * msg = new LandmarkBeaconMessage(l.id,l.position,l.distance);
    p2p->send(msg);
  }
}

bool Landmarks::update(LandmarkEntry &l) {
  vector<LandmarkEntry>::iterator it;
  bool kept = false;
  // Update or insert database record

  // Try to locate record in the database
  for (it = landmarks.begin(); it < landmarks.end(); it++) {
    if (it->id == l.id) {
      // entry found
      break;
    }
  }
  
  if (it == landmarks.end()) {
    // Not found
    if (landmarks.size() < MAX_NUM_LANDMARKS) {
      // but enough space to store that landmark
      landmarks.push_back(l);
      kept = true;
    }
  } else {
    // found
    // ignore time for now
    if (l.distance < it->distance) {
      // update distance
      it->distance = l.distance;
      kept = true;
    }
  }
  return kept;
}

bool Landmarks::handle(MessagePtr message) {
  P2PNetworkInterface * recv_interface = message->destinationInterface;

  switch(message->type) {
  case LANDMARK_BEACON_MSG:
    {
      LandmarkBeaconMessage_ptr m = boost::static_pointer_cast<LandmarkBeaconMessage>(message);
      LandmarkEntry l(m->id,m->distance,m->position,0);
      bool isNew = update(l);

      if(isNew) {
	// Broadcast message (distance+1)
	l.distance++;
	advertise(l,recv_interface);
      }
      return true;
    }
  default:
    return false;
  }

  return false;
}

void Landmarks::handle(EventPtr pev) {
  switch (pev->eventType) {
    case LANDMARK_BEACON_EVENT:
      {
	// Broadcast info  
	LandmarkEntry l(blockCode->hostBlock->blockId, 1, blockCode->map->getPosition(), 0);
	advertise(l,NULL);
#ifdef LANDMARKS_DEBUG
	cout << "@" << blockCode->hostBlock->blockId << ", t " << pev->date << ": periodic landmark beacon" << endl;
#endif
	// Reschedule the event (periodic for now)
	// check list interface catoms2d
	if (!mustStop) {
	  BaseSimulator::Scheduler *scheduler =  BaseSimulator::getScheduler();
	  scheduleAdvertise(scheduler->now()+SEC_TO_MS(LANDMARK_BEACON_PERIOD_SEC));
	}
      }
      break;
  default:
    cerr << "Landmarks event handler: unknown event type" << endl;
  }
}

void Landmarks::stop() {
  mustStop = true;
}
