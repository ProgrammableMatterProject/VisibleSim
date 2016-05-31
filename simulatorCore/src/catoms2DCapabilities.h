/*
 * catoms2DCapability.h
 *
 *  Created on: 15 janvier 2014
 *      Author: Benoît
 */
#ifndef CATOMS2DCAPABILITY_H_
#define CATOMS2DCAPABILITY_H_

#include <vector>
#include <climits>
#include "vecteur.h"
#include "trace.h"
#include <string>
#include <TinyXML/tinyxml.h>

namespace Catoms2D {

class PointRel3D {
public :
	short x;
	short y;
	short z;
	PointRel3D();
	PointRel3D(short vx,short vy,short vz);
	PointRel3D(const string &str);

	void set(short vx,short vy,short vz);
	bool isInBoundingBox(short x0,short x1,short y0,short y1,short z0,short z1);
	void fromString(const string &str);
	void fromString(const string &str,const PointRel3D&ref);
	PointRel3D rotateY(int n) const;
	PointRel3D rotateZ(int n) const;
	inline bool isZero() { return (x==0 && y==0 && z==0); }
	inline bool isSet() const { return x!=SHRT_MIN; }
	friend bool operator ==(const PointRel3D p1,const PointRel3D p2);
	friend bool operator !=(const PointRel3D p1,const PointRel3D p2);
	friend ostream& operator<<(ostream& f,const PointRel3D &pt);
};

enum states { emptyState=0, fullState=3, filledState=5, emptiedState=6, transitState=7, jockerState=8 };
enum presence { emptyCell=0, fullCell=1, wallCell=2, lockedCell=3};

class PresenceMatrix {
public :
    presence grid[27];
    PresenceMatrix();
	friend ostream& operator<<(ostream& f,const PresenceMatrix &sm);
	void add(presence v,const PointRel3D &posAdd,const PointRel3D &origine);
	void set(const PointRel3D& pt,presence v);
	presence get(const PointRel3D &pt) const;
};

class ValidationMatrix {
public :
	states grid[27];
	ValidationMatrix();

	void fromString(const char *str);
	void fromRotationY(const ValidationMatrix&,int);
	void fromRotationZ(const ValidationMatrix&,int);

	void set(const PointRel3D& pt,states v);
	states get(const PointRel3D &pt);
	friend ostream& operator<<(ostream& f,const ValidationMatrix &sm);
	bool validate(const PresenceMatrix &pm);
	bool validateWithTarget(const PresenceMatrix &pm,const PresenceMatrix &ltm,int &gain);
	bool validateTrain(const PresenceMatrix &pm,const PresenceMatrix &ltm,int &gain);
};

class Motion {
public :
	Motion();
	Motion(const Motion *);
	int obj;
	double endTime;
	PointRel3D init;
	PointRel3D final;
};

class Capability {
public :
	int num;
	short level;
	string name;
	ValidationMatrix validationMatrix;
	PointRel3D refPos;//,*helperPos
	PointRel3D *linkNextPos,*linkPrevPos;
	bool isHead,isEnd;
	vector <Motion*> tabMotions;
	short nbreConditions;
	PointRel3D conditionPreviousPosition;

	Capability(const string& sid);
	Capability(const string& sid,const Capability*ref,int n);
	~Capability();

	friend ostream& operator<<(ostream& f,const Capability &c);
	void getCenter(PointRel3D &center);
  PointRel3D getMotionVector(int id,int time);
  PointRel3D getMotionVector(int id);
};

class Validation {
public:
	Capability*capa;
	short gain;
	short nbreConditions;
	Validation(Capability *c,short g,short n);
};

class Catoms2DCapabilities {
public:
	vector <Capability*> tabCapabilities;

	Catoms2DCapabilities(TiXmlNode *node);
	~Catoms2DCapabilities();
  Capability *validate(const PresenceMatrix &sm);
  Capability *validateWithTarget(const PresenceMatrix &sm,const PresenceMatrix &ltm,int &gain);
  Capability *validateTrain(const PresenceMatrix &sm,const PresenceMatrix &ltm,int &gain);
  vector <Validation*> *validateMulti(const PresenceMatrix &sm,const PresenceMatrix &ltm);
};

}
#endif
