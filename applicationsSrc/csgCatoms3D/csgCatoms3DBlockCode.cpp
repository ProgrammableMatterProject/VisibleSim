/*
 * csgCatoms3DBlockCode.cpp
 *
 *  Created on: 05 August 2015
 *  Author: Thadeu
 */

#include <iostream>
#include <sstream>
#include "csgCatoms3DBlockCode.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "events.h"
#include <boost/shared_ptr.hpp>
#include <chrono>
#include <fstream>
#define MAX 1000
#define INF 99999

using namespace std;
using namespace Catoms3D;

int CsgCatoms3DBlockCode::difference_bitmap = 0;
int CsgCatoms3DBlockCode::difference_mesh = 0;
int CsgCatoms3DBlockCode::difference_stoy = 0;
int CsgCatoms3DBlockCode::total_csg = 0;
double CsgCatoms3DBlockCode::bitmap_time_elapsed = 0;
double CsgCatoms3DBlockCode::csg_time_elapsed = 0;
double CsgCatoms3DBlockCode::stoy_time_elapsed = 0;
double CsgCatoms3DBlockCode::mesh_time_elapsed = 0;
int CsgCatoms3DBlockCode::side_size = 30;
bool CsgCatoms3DBlockCode::bitmap[27000] = {0};

CsgCatoms3DBlockCode::CsgCatoms3DBlockCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
	cout << "CsgCatoms3DBlockCode constructor" << endl;
	scheduler = Catoms3D::getScheduler();
	catom = (Catoms3DBlock*)hostBlock;
    distance = INF;
}

CsgCatoms3DBlockCode::~CsgCatoms3DBlockCode() {
	cout << "CsgCatoms3DBlockCode destructor" << endl;
}

void CsgCatoms3DBlockCode::generateBitmap() {
    int x = catom->position.pt[0];
    int y = catom->position.pt[1];
    int z = catom->position.pt[2];
    int pos = x + y*side_size + z*side_size*side_size;
    bitmap[pos] = csgUtils.isInside(myPosition).isInside();
}

void CsgCatoms3DBlockCode::startup() {
	stringstream info;

	info << "Starting  ";

    Vecteur basePosition(4, 4, 4);
    info << "POSITION = " << catom->position << endl;
	scheduler->trace(info.str(),hostBlock->blockId);
    hasPosition = false;

	if (catom->blockId==1) {
        distance = 0;
        csgUtils.readFile("data/mug-color.bc");
//        stoyUtils.readFile("data/sphere-high.stoy");
        meshUtils.readFile("data/voiture.obj");
    //    bitmapUtils.readFile("data/sphere.bmp");


        PositionInfo pi = csgUtils.isInside(myPosition);
        if (pi.isInside()) {
            catom->setColor(pi.getColor());
        }
        else
            catom->setVisible(false);

        myPosition = Vecteur(0, 0, 0);
        hasPosition = true;
        sendCSGMessage();
        
	}
}


void CsgCatoms3DBlockCode::processLocalEvent(EventPtr pev) {
	MessagePtr message;
	stringstream info;

	switch (pev->eventType) {
    case EVENT_NI_RECEIVE: {
      message = (boost::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
        switch(message->id) {
            case DISTANCE_MSG_ID:
            {
                Distance_message_ptr recv_message = boost::static_pointer_cast<Distance_message>(message);
                vector<Color> listColors;
                listColors.push_back(WHITE);
                listColors.push_back(RED);
                listColors.push_back(GREEN);
                listColors.push_back(LIGHTGREEN);
                listColors.push_back(BLUE);
                listColors.push_back(YELLOW);
                listColors.push_back(CYAN);
                listColors.push_back(MAGENTA);
                if (recv_message->getDistance() < distance) {
                    distance = recv_message->getDistance();
                    catom->setColor(listColors[distance%listColors.size()]);
                    sendDistanceMessage();

                }
            }
            case CSG_MSG_ID:
            {
                if (!hasPosition) {
                    catom->setColor(PINK);
                    CSG_message_ptr recv_message = boost::static_pointer_cast<CSG_message>(message);

                    char *csgBuffer = recv_message->getCsgBuffer();
                    int csgBufferSize = recv_message->getCsgBufferSize();
                    csgUtils.readCSGBuffer(csgBuffer, csgBufferSize);
                    stoyUtils.setBricks(recv_message->getBricks());
                    bitmapUtils.setBitmap(recv_message->getBitmap());

                    myPosition = recv_message->getPosition();

                    PositionInfo pi = csgUtils.isInside(myPosition);
                    //PositionInfo pi;
                    if (pi.isInside()) {
                        catom->setColor(pi.getColor());
                    }
                    else 
                        //catom->setColor(PINK);
                        catom->setVisible(false);


                    hasPosition = true;
                    sendCSGMessage();
                }
                break;
            }
          }
      }
      break;
	}
}
/*
void CsgCatoms3DBlockCode::methodsDifference() {
    if (csgUtils.isInside(myPosition) != bitmapUtils.isInside(catom->position, side_size))
        difference_bitmap++;
    if (csgUtils.isInside(myPosition) != stoyUtils.isInside(myPosition))
        difference_stoy++;
    if (csgUtils.isInside(myPosition) != meshUtils.isInside(myPosition))
        difference_mesh++;
    if (csgUtils.isInside(myPosition))
        total_csg++;
}

void CsgCatoms3DBlockCode::benchmark() {
    calcBitmap();
    calcCSG();
    calcStoy();
    calcMesh();
}

void CsgCatoms3DBlockCode::calcBitmap() {
    auto begin = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < MAX; i++) {
        bitmapUtils.isInside(catom->position, side_size);
    }
    auto end = std::chrono::high_resolution_clock::now();
    bitmap_time_elapsed += (std::chrono::duration_cast<std::chrono::nanoseconds>(end-begin).count())/MAX;

    cout << std::chrono::duration_cast<std::chrono::nanoseconds>(end-begin).count()/MAX << ";";
}

void CsgCatoms3DBlockCode::calcCSG() {
    auto begin = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < MAX; i++) {
        csgUtils.isInside(myPosition);
    }
    auto end = std::chrono::high_resolution_clock::now();
    csg_time_elapsed += (std::chrono::duration_cast<std::chrono::nanoseconds>(end-begin).count())/MAX;

    cout << std::chrono::duration_cast<std::chrono::nanoseconds>(end-begin).count()/MAX << ";";
}

void CsgCatoms3DBlockCode::calcStoy() {
    auto begin = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < MAX; i++) {
        stoyUtils.isInside(myPosition);
    }
    auto end = std::chrono::high_resolution_clock::now();
    stoy_time_elapsed += (std::chrono::duration_cast<std::chrono::nanoseconds>(end-begin).count())/MAX;

    cout << std::chrono::duration_cast<std::chrono::nanoseconds>(end-begin).count()/MAX << ";";
}

void CsgCatoms3DBlockCode::calcMesh() {
    auto begin = std::chrono::high_resolution_clock::now();
    for (int i = 0; i < MAX; i++) {
        meshUtils.isInside(myPosition);
    }
    auto end = std::chrono::high_resolution_clock::now();
    mesh_time_elapsed += (std::chrono::duration_cast<std::chrono::nanoseconds>(end-begin).count())/MAX;

    cout << std::chrono::duration_cast<std::chrono::nanoseconds>(end-begin).count()/MAX << endl;
}
*/
void CsgCatoms3DBlockCode::sendDistanceMessage() {
    for (int i = 0; i < 12; i++) {
        if (catom->getInterface(i)->connectedInterface != NULL) {
            Vecteur pos(
                myPosition.pt[0] + Catoms3D::tabConnectorPositions[i][0], 
                myPosition.pt[1] + Catoms3D::tabConnectorPositions[i][1],
                myPosition.pt[2] + Catoms3D::tabConnectorPositions[i][2]);
            Distance_message *message = new Distance_message(distance+1);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, message, catom->getInterface(i)));
        }
    }
}

void CsgCatoms3DBlockCode::sendCSGMessage() {
    for (int i = 0; i < 12; i++) {
        if (catom->getInterface(i)->connectedInterface != NULL) {
            Vecteur pos(
                myPosition.pt[0] + Catoms3D::tabConnectorPositions[i][0], 
                myPosition.pt[1] + Catoms3D::tabConnectorPositions[i][1],
                myPosition.pt[2] + Catoms3D::tabConnectorPositions[i][2]);
            CSG_message *message = new CSG_message(csgUtils.getCSGBuffer(), csgUtils.getCSGBufferSize(), stoyUtils.getBricks(), bitmapUtils.getBitmap(), pos);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + 100, message, catom->getInterface(i)));
        }
    }
}


Catoms3DBlockCode* CsgCatoms3DBlockCode::buildNewBlockCode(Catoms3DBlock *host) {
	return(new CsgCatoms3DBlockCode(host));
}

Distance_message::Distance_message(int _distance) {
    id = DISTANCE_MSG_ID;
    distance = _distance;
}

CSG_message::CSG_message(char *_csgBuffer, int _csgBufferSize, vector<Brick> _bricks, string _bitmap, Vecteur pos) {
	id = CSG_MSG_ID;

    csgBuffer = new char[_csgBufferSize];
    memcpy(csgBuffer, _csgBuffer, _csgBufferSize);

    csgBufferSize = _csgBufferSize;

    bricks = _bricks;
    bitmap = _bitmap;
    position = pos;
}

CSG_message::~CSG_message() {
    delete[] csgBuffer;
}


