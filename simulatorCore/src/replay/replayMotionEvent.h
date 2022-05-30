//
// Created by daluz on 04/02/2021.
//

#pragma once

#include "../utils/utils.h"
#include "../base/simulator.h"
#include "replayTags.h"
#include "math/cell3DPosition.h"
using namespace ReplayTags;
class ReplayMotionEvent {
public:
    ReplayMotionEvent(Time date, bID bid, Time duration_us,Cell3DPosition destination, Cell3DPosition origin);

    virtual void write(ofstream* exportFile, ofstream* debugFile, bool debug);

    ~ReplayMotionEvent();

    Time getEndDate(){return date+duration;}
protected:
    Time date;
    bID bid;
    Time duration;
    Cell3DPosition origin;
    Cell3DPosition destination;


};
