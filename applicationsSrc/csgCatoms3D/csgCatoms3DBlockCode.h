/*
 * csgCatoms3DBlockCode.h
 *
 *  Created on: 06 august 2015
 *  Author: Thadeu
 */

#ifndef CSGCATOMS3DBLOCKCODE_H_
#define CSGCATOMS3DBLOCKCODE_H_

#define CSG_MSG_ID	9001
#define DISTANCE_MSG_ID	9002

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"

#include "rotation3DEvents.h"
#include "catoms3DBlock.h"
#include "scheduler.h"
#include "events.h"
#include "csgUtils.h"
#include "stoyUtils.h"
#include "meshUtils.h"
#include "bitmapUtils.h"

class CSG_message;
class Distance_message;

typedef std::shared_ptr<CSG_message> CSG_message_ptr;
typedef std::shared_ptr<Distance_message> Distance_message_ptr;

class CsgCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
    /* Debug Info */
    static int difference_bitmap;
    static int difference_mesh;
    static int difference_stoy;
    static int total_csg;
    static double bitmap_time_elapsed;
    static double csg_time_elapsed;
    static double stoy_time_elapsed;
    static double mesh_time_elapsed;
    static int side_size;
    static bool bitmap[27000];

	Scheduler *scheduler;
	Catoms3D::Catoms3DBlock *catom;
    Vector3D myPosition; // has relative position from the master
    bool hasPosition; // flag position
    int distance;
    CsgUtils csgUtils;
    StoyUtils stoyUtils;
    MeshUtils meshUtils;
    BitmapUtils bitmapUtils;

	CsgCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
	~CsgCatoms3DBlockCode();

	void startup();
	void processLocalEvent(EventPtr pev);
    void createCSG();
    void sendCSGMessage();
    void sendDistanceMessage();

    void benchmark();
    void calcBitmap();
    void calcCSG();
    void calcStoy();
    void calcMesh();
    void methodsDifference();
    void generateBitmap();

	static Catoms3D::Catoms3DBlockCode *buildNewBlockCode(Catoms3D::Catoms3DBlock *host);

};

class Distance_message : public Message {
    int distance;
public :
    Distance_message(int _dist);
	int getDistance() { return distance; };
};

class CSG_message : public Message {
    char *csgBuffer;
    int csgBufferSize;
    Vector3D position;
    vector<Brick> bricks;
    string bitmap;
public :
	CSG_message(char *_csgBuffer, int _csgBufferSize, vector<Brick> bricks, string _bitmap, Vector3D position);
	~CSG_message();

	char* getCsgBuffer() { return csgBuffer; };
	int getCsgBufferSize() { return csgBufferSize; };
	vector<Brick> getBricks() { return bricks; };
	string getBitmap() { return bitmap; };
	Vector3D getPosition() { return position; };
};


#endif /* CSGCATOMS3DBLOCKCODE_H_ */
