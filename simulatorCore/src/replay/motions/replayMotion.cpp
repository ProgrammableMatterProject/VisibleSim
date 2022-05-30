//
// Created by daluz on 04/02/2021.
//

#include "replayMotion.h"

ReplayMotion::ReplayMotion(Time date, bID bid, Time duration_us,
                           const Cell3DPosition& destination, Cell3DPosition &origin) :
ReplayMotionEvent(date, bid, duration_us, destination, origin)
{

}


void ReplayMotion::write(ofstream* exportFile, ofstream* debugFile, bool debug)
{
    exportFile->write((char*)&date, sizeof(Time));
    exportFile->write((char*)&EVENT_MOTION, sizeof(u1));
    exportFile->write((char*)&bid, sizeof(bID));
    exportFile->write((char*)&duration, sizeof(u8));
    exportFile->write((char*)&origin.pt, 3*sizeof(u2));
    exportFile->write((char*)&destination.pt,3*sizeof(u2));
    //exportFile->write((char*)&finalOrientation,sizeof(u2));
    if (debug) {
        *debugFile << "Motion:" << date << " " << (int)EVENT_MOTION << " " << bid
                   << " " << (int)duration << " " <<
                   origin[0]<<" " << origin[1]<<" "<<origin[2]<< " TO " << destination[0] << " " << destination[1] << " " << destination[2] << endl;
    }
}

ReplayMotion::~ReplayMotion() {

}
