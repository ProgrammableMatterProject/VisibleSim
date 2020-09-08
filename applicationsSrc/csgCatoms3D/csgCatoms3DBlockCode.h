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

#include "robots/catoms3D/catoms3DBlockCode.h"
#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DBlock.h"
#include "events/scheduler.h"
#include "events/events.h"
//#include "csg/csgUtils.h"
#include "stoyUtils.h"
#include "meshUtils.h"
#include "bitmapUtils.h"
#include "grid/target.h"

class CSG_message;

typedef std::shared_ptr<CSG_message> CSG_message_ptr;

class CsgCatoms3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
    static bool bitmap[27000]; // used to generate the bitmap file from any method in use
    static int side_size; // used to read the bitmap vector as a matrix

    Scheduler *scheduler;
    Catoms3D::Catoms3DBlock *catom;
    Vector3D myPosition; // has relative position from the master
    bool hasPosition; // flag position
    StoyUtils stoyUtils;
    MeshUtils meshUtils;
    BitmapUtils bitmapUtils;

    CsgCatoms3DBlockCode(Catoms3D::Catoms3DBlock *host);
    ~CsgCatoms3DBlockCode();

    void startup() override;
    void processLocalEvent(EventPtr pev) override;
    void createCSG();
    void sendCSGMessage();

    void benchmark();
    void calcBitmap();
    void calcCSG();
    void calcStoy();
    void calcMesh();
    void methodsDifference();
    void generateBitmap(int side_size);

    static BlockCode *buildNewBlockCode(BuildingBlock *host);

    virtual void onGlDraw() override;
};

class CSG_message : public Message {
    //char *csgBuffer;
    Vector3D position;
    vector<Brick> bricks;
    string bitmap;
public :
    CSG_message(vector<Brick> bricks, string _bitmap, Vector3D position);
    ~CSG_message();

    //char* getCsgBuffer() { return csgBuffer; };
    vector<Brick> getBricks() { return bricks; };
    string getBitmap() { return bitmap; };
    Vector3D getPosition() { return position; };
};

class CsgCatoms3DStats {

public:

    /* time info */
    static int difference_bitmap;
    static int difference_mesh;
    static int difference_stoy;
    static int total_csg;
    static double bitmap_time_elapsed;
    static double csg_time_elapsed;
    static double stoy_time_elapsed;
    static double mesh_time_elapsed;
};


#endif /* CSGCATOMS3DBLOCKCODE_H_ */
