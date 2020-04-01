/*
 * smartBlocksCapability.h
 *
 *  Created on: 15 janvier 2014
 *      Author: Benoît
 */
#ifndef SMARTBLOCKSCAPABILITY_H_
#define SMARTBLOCKSCAPABILITY_H_

#include <vector>
#include <climits>
#include "math/vector3D.h"
#include "utils/trace.h"
#include <string>
#include "deps/TinyXML/tinyxml.h"

class PointCel {
public :
    short x;
    short y;
    PointCel();
    PointCel(short vx,short vy);
    PointCel(const char *str);
    PointCel(const char *str,const PointCel&ref);

    void set(short vx,short vy);
    void set(const char *str);
    void fromString(const char *str);
    //void fromString(const string &str);
    void fromString(const string &str,const PointCel&ref);

    PointCel rotateZ(int n) const;
    inline bool isZero() { return (x==0 && y==0); }
    inline bool isSet() const { return x!=SHRT_MIN; }
    inline void unSet() { x=SHRT_MIN; y=SHRT_MIN; }
    friend bool operator ==(const PointCel p1,const PointCel p2);
    friend bool operator !=(const PointCel p1,const PointCel p2);
    friend ostream& operator<<(ostream& f,const PointCel &pt);
};

enum states { emptyState=0, wellPlaceState=1, fullState=3, singleState=4, filledState=5, emptiedState=6, transitState=7, jockerState=8 };
enum presence { emptyCell=0, fullCell=1, wallCell=2, borderCell=3, singleCell=4};

class PresenceMatrix {
public :
    presence grid[9];
    PresenceMatrix();
    friend ostream& operator<<(ostream& f,const PresenceMatrix &sm);
    void add(presence v,const PointCel &posAdd,const PointCel &origine);
    void set(const PointCel& pt,presence v);
    presence get(const PointCel &pt) const;
    presence get(short x,short y) const;

    bool isBorder();
};

class ValidationMatrix {
public :
    states grid[9];
    ValidationMatrix();

    void fromString(const char *str);
    void fromRotationZ(const ValidationMatrix&,int);

    void set(const PointCel& pt,states v);
    states get(const PointCel &pt);
    friend ostream& operator<<(ostream& f,const ValidationMatrix &sm);
    bool validate(const PresenceMatrix &pm,const bool tt[9][5]);
    bool validateWithTarget(const PresenceMatrix &pm,const PresenceMatrix &ltm,int &gain);
    bool validateTrain(const PresenceMatrix &pm,const PresenceMatrix &ltm,int &gain);
};

class Motion {
public :
    Motion();
    Motion(const Motion *);
    vector <short> PathToBlock;
    vector <short> UnlockPath;
    double time;
    PointCel vect;
};

class Capability {
public :
    string name;
    ValidationMatrix validationMatrix;
    PointCel *linkNextPos,*linkPrevPos;
    bool isHead,isEnd;
    vector <Motion*> tabMotions;
    bool isAngle;
    vector <short> tabUnlockPath;
    PointCel prevDir,*singleMotionDir;

    Capability(const string& sid);
    Capability(const string& sid,const Capability*ref,int n);
    ~Capability();

    friend ostream& operator<<(ostream& f,const Capability &c);
    void getCenter(PointCel &center);
};

class Validation {
public:
    Capability*capa;
    short gain;
    bool isZeroDistance;
    Validation(Capability *c,short g,bool izd=false);
};

class SmartBlocksCapabilities {
public:
    vector <Capability*> tabCapabilities;

    SmartBlocksCapabilities(TiXmlNode *node);
    ~SmartBlocksCapabilities();
    vector <Validation*> *validateDirection(const PresenceMatrix &sm,const PresenceMatrix &ltm,
                                            PointCel dir[4]);
//	vector <Validation*> *validateMulti(const PresenceMatrix &sm,const PresenceMatrix &ltm);
    vector <Validation*> *validateToTarget(const PresenceMatrix &sm,const PointCel &dirToO);
};

#endif
