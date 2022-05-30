//
// Created by daluz on 04/02/2021.
//

#include "replayMotionEvent.h"
#include "replayTags.h"



ReplayMotionEvent::ReplayMotionEvent(Time date, bID bid, Time duration_us,Cell3DPosition destination, Cell3DPosition origin) {

    this->date = date;
    this->bid = bid;
    this->duration = duration_us;
    this->destination = destination;
    this->origin = origin;
}

ReplayMotionEvent::~ReplayMotionEvent() {

}

void ReplayMotionEvent::write(ofstream* exportFile, ofstream* debugFile, bool debug) {
    
}


