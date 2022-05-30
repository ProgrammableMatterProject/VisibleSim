//
// Created by daluz on 04/02/2021.
//


#pragma  once
#include "../replayMotionEvent.h"

class ReplayMotion : public ReplayMotionEvent{
public:
    ReplayMotion(Time date, bID bid, Time duration_us,const Cell3DPosition& destination, Cell3DPosition& origin);

    void write(ofstream* exportFile, ofstream* debugFile, bool debug) override;

    virtual ~ReplayMotion();

private:

};


