#ifndef _capability_h
#define _capability_h

#include <vector>
#include "vecteur.h"
#include <string>

class Block; 

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
	friend ostream& operator<<(ostream& f,const PointRel3D &pt);
	friend bool operator <=(const PointRel3D p1,const PointRel3D p2);
	friend bool operator >(const PointRel3D p1,const PointRel3D p2);
	friend bool operator !=(const PointRel3D p1,const PointRel3D p2);
};

enum states { emptyState=0, fullState=1, anyState=2, zeroToOneState=3, oneToZeroState=4, wallState=5, lockedEmptyState=6, lockedFullState=7};

class StateMatrix {
public :
	states grid[27];
	StateMatrix();

	void fromString(const char *str);
	void fromRotationY(const StateMatrix&,int);
	void fromRotationZ(const StateMatrix&,int);
	bool isCompatible(const StateMatrix&);
	void set(const PointRel3D& pt,states v);
	void lock(const PointRel3D& pt);
	states get(const PointRel3D &pt);
	friend ostream& operator<<(ostream& f,const StateMatrix &sm);
	int distance(const StateMatrix &sm);
	int conditionalDistance(const StateMatrix &sm,PointRel3D &pos,states &value);
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
	string name;
/*	PointRel3D initRelPos;
	PointRel3D finalRelPos;*/
	StateMatrix stateMatrix;
	PointRel3D refPos;
	vector <Motion*> tabMotions;

	Capability(const string& sid);
	Capability(const string& sid,const Capability*ref,int n);
	~Capability();

	//bool isCompatible(const StateMatrix&sm);
	friend ostream& operator<<(ostream& f,const Capability &c);
	void getCenter(PointRel3D &center);
};

class Capabilities {
public:
	vector <Capability*> tabCapabilities;

	Capabilities(const char *title);
	~Capabilities();

};

#endif
