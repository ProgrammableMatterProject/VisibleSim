//
// Created by daluz on 04/02/2021.
//

#include "replayCatoms3DMotion.h"

ReplayCatoms3DMotion::ReplayCatoms3DMotion(Time date, bID bid, Time duration_us,Cell3DPosition destination,
                                           short orientation, Cell3DPosition origin, short originOrientation,
                                           u4 fixedBlockId, u1 type, Vector3D axe1, Vector3D axe2) :
ReplayMotionEvent(date, bid,duration_us,destination, origin)
{
    this->fixedBlockId = fixedBlockId;
    this->type = type;
    this->axe1 = axe1;
    this->axe2 = axe2;
    this->finalOrientation = orientation;
    this->originOrientation = originOrientation;

}

ReplayCatoms3DMotion::~ReplayCatoms3DMotion() {

}

void ReplayCatoms3DMotion::write(ofstream* exportFile, ofstream* debugFile, bool debug)
{
    exportFile->write((char*)&date, sizeof(u8));
    exportFile->write((char*)&EVENT_MOTION_CATOMS3D, sizeof(u1));
    exportFile->write((char*)&bid, sizeof(u4));

    exportFile->write((char*)&duration, sizeof(u8));
    exportFile->write((char*)&origin.pt, 3*sizeof(u2));
    exportFile->write((char*)&originOrientation, sizeof(u2));
    exportFile->write((char*)&destination.pt, 3*sizeof(u2));
    exportFile->write((char*)&finalOrientation, sizeof(u2));
    exportFile->write((char*)&fixedBlockId, sizeof(u4));
    exportFile->write((char*)&type, sizeof(u1));
    //exportFile->write((char*)&duration, sizeof(u8));
    cout<<"WRITING EVENT MOTION : AXE1 = "<<axe1<<endl;
    exportFile->write((char*)axe1.getPtr(), 3*sizeof(u4));
    /*exportFile->write((char*)&axe1.pt[1], sizeof(u2));
    exportFile->write((char*)&axe1.pt[2], sizeof(u2));*/

    exportFile->write((char*)axe2.getPtr(), 3*sizeof(u4));
    /*exportFile->write((char*)&axe2.pt[1], sizeof(u2));
    exportFile->write((char*)&axe2.pt[2], sizeof(u2));*/

    if (debug) {
        *debugFile << "MotionC3D:" << date << " " << (int)EVENT_MOTION_CATOMS3D << " " << bid
                   << " " << (int)duration << " " << " " << destination[0] << " " << destination[1] << " " << destination[2]
                   << " FixedBlockId : "<<fixedBlockId<<endl;
    }
}