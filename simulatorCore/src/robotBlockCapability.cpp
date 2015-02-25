#include "capability.h"
#include "block.h"
#include "tinyxml.h"

static int localNum=0;
	
Capability::Capability(const string& sid):name(sid) {
	cout << "new capability: "<< name << endl;
	num=localNum++;
}

Capability::Capability(const string& sid,const Capability*ref,int n):name(sid) {
	num=localNum++;

	cout << "new capaility: "<< name << endl;
	StateMatrix tmp,tmp2;
	PointRel3D tmppt;

	switch (n) {
		case 1 : case 2 : case 3 : // rotation around Z axis
			
			stateMatrix.fromRotationZ(ref->stateMatrix,n);
			refPos = ref->refPos.rotateZ(n);
/*			initRelPos = ref->initRelPos.rotateZ(n);
			finalRelPos = ref->finalRelPos.rotateZ(n);*/

			// copy the tabMotion vector with rotation
			vector <Motion*>::const_iterator cm = ref->tabMotions.begin();
			Motion *nm;
			while (cm!=ref->tabMotions.end()) {
				nm = new Motion(*cm);
				nm->init = (*cm)->init.rotateZ(n);
				nm->final = (*cm)->final.rotateZ(n);
				tabMotions.push_back(nm);
				cm++;
			}

		break;
/*		case 4 : case 5 : case 6 : case 7 : 
			tmp.fromRotationY(ref->init,1);
			init.fromRotationZ(tmp,n-4);
			tmppt = ref->borderRelPos.rotateY(1);
			borderRelPos = tmppt.rotateZ(n-4);
			tmppt = ref->neighborRelPos.rotateY(1);
			neighborRelPos = tmppt.rotateZ(n-4);
			tmppt = ref->finalRelPos.rotateY(1);
			finalRelPos = tmppt.rotateZ(n-4);
			if (ref->friendRelInitPos) {
				tmppt = ref->friendRelInitPos->rotateY(1);
				friendRelInitPos=new PointRel3D(tmppt.rotateZ(n-4));
			} else friendRelInitPos=NULL;
			if (ref->friendRelFinalPos) {
				tmppt = ref->friendRelFinalPos->rotateY(1);
				friendRelFinalPos=new PointRel3D(tmppt.rotateZ(n-4));
			} else friendRelFinalPos=NULL;
		break;
		case 8 : case 9 : case 10 : case 11 : 
			tmp.fromRotationY(ref->init,2);
			init.fromRotationZ(tmp,n-8);
			tmppt = ref->borderRelPos.rotateY(2);
			borderRelPos = tmppt.rotateZ(n-8);
			tmppt = ref->neighborRelPos.rotateY(2);
			neighborRelPos = tmppt.rotateZ(n-8);
			tmppt = ref->finalRelPos.rotateY(2);
			finalRelPos = tmppt.rotateZ(n-8);
			if (ref->friendRelInitPos) {
				tmppt = ref->friendRelInitPos->rotateY(2);
				friendRelInitPos=new PointRel3D(tmppt.rotateZ(n-8));
			} else friendRelInitPos=NULL;
			if (ref->friendRelFinalPos) {
				tmppt = ref->friendRelFinalPos->rotateY(2);
				friendRelFinalPos=new PointRel3D(tmppt.rotateZ(n-8));
			} else friendRelFinalPos=NULL;
		break;
		case 12 : case 13 : case 14 : case 15 : 
			tmp.fromRotationY(ref->init,3);
			init.fromRotationZ(tmp,n-12);
			tmppt = ref->borderRelPos.rotateY(3);
			borderRelPos = tmppt.rotateZ(n-12);
			tmppt = ref->neighborRelPos.rotateY(3);
			neighborRelPos = tmppt.rotateZ(n-12);
			tmppt = ref->finalRelPos.rotateY(3);
			finalRelPos = tmppt.rotateZ(n-12);
			if (ref->friendRelInitPos) {
				tmppt = ref->friendRelInitPos->rotateY(3);
				friendRelInitPos=new PointRel3D(tmppt.rotateZ(n-12));
			} else friendRelInitPos=NULL;
			if (ref->friendRelFinalPos) {
				tmppt = ref->friendRelFinalPos->rotateY(3);
				friendRelFinalPos=new PointRel3D(tmppt.rotateZ(n-12));
			} else friendRelFinalPos=NULL;
		break;
		case 16 : case 17 : case 18 : case 19 :
			tmp.fromRotationZ(ref->init,1);
			tmp2.fromRotationY(tmp,3);
			init.fromRotationZ(tmp2,n-16);
			tmppt = ref->borderRelPos.rotateZ(1);
			tmppt = tmppt.rotateY(3);
			borderRelPos = tmppt.rotateZ(n-16);
			tmppt = ref->neighborRelPos.rotateZ(1);
			tmppt = tmppt.rotateY(3);
			neighborRelPos = tmppt.rotateZ(n-16);
			tmppt = ref->finalRelPos.rotateZ(1);
			tmppt = tmppt.rotateY(3);
			finalRelPos = tmppt.rotateZ(n-16);
			if (ref->friendRelInitPos) {
				tmppt = ref->friendRelInitPos->rotateZ(1);
				tmppt = tmppt.rotateY(3);
				friendRelInitPos=new PointRel3D(tmppt.rotateZ(n-16));
			} else friendRelInitPos=NULL;
			if (ref->friendRelFinalPos) {
				tmppt = ref->friendRelFinalPos->rotateZ(1);
				tmppt = tmppt.rotateY(3);
				friendRelFinalPos=new PointRel3D(tmppt.rotateZ(n-16));
			} else friendRelFinalPos=NULL;
		break;
		case 20 : case 21 : case 22 : case 23 :
			tmp.fromRotationZ(ref->init,3);
			tmp2.fromRotationY(tmp,3);
			init.fromRotationZ(tmp2,n-20);
			tmppt = ref->borderRelPos.rotateZ(3);
			tmppt = tmppt.rotateY(3);
			borderRelPos = tmppt.rotateZ(n-20);
			tmppt = ref->neighborRelPos.rotateZ(3);
			tmppt = tmppt.rotateY(3);
			neighborRelPos = tmppt.rotateZ(n-20);
			tmppt = ref->finalRelPos.rotateZ(3);
			tmppt = tmppt.rotateY(3);
			finalRelPos = tmppt.rotateZ(n-20);
			if (ref->friendRelInitPos) {
				tmppt = ref->friendRelInitPos->rotateZ(3);
				tmppt = tmppt.rotateY(3);
				friendRelInitPos=new PointRel3D(tmppt.rotateZ(n-20));
			} else friendRelInitPos=NULL;
			if (ref->friendRelFinalPos) {
				tmppt = ref->friendRelFinalPos->rotateZ(3);
				tmppt = tmppt.rotateY(3);
				friendRelFinalPos=new PointRel3D(tmppt.rotateZ(n-20));
			} else friendRelFinalPos=NULL;
		break;*/
	}
}
	

Capability::~Capability() {
	vector <Motion *>::const_iterator ci = tabMotions.begin();
	while (ci!=tabMotions.end()) {
		delete (*ci);
		ci++;
	}
	tabMotions.clear();
}
/*
bool Capability::isCompatible(const StateMatrix &sm) {
	int i;

	for (i=0; i<27; i++) {
		if (((sm.grid[i]==emptyState || sm.grid[i]==zeroToOneState) && (states.grid[i]==fullState || states.grid[i]==oneToZeroState) )||
			((sm.grid[i]==fullState || sm.grid[i]==oneToZeroState) && (states.grid[i]==emptyState || states.grid[i]==zeroToOneState))) return false;
	}
	return true;
}
*/
// write capability parameters into a flux
ostream& operator<<(ostream& f,const Capability &c) { 
	f << "Capability:" << c.name << endl;
	f << "Ref pos: " << c.refPos << endl;
	f << "States condition: " << endl;
	f << c.stateMatrix;
/*	f << "From : " << c.initRelPos << endl;
	f << "To : " << c.finalRelPos << endl;*/
  return f;
}

void Capability::getCenter(PointRel3D &center) {
	center.x = -refPos.x;
	center.y = -refPos.y;
	center.z = -refPos.z;
}

/*************************************BPi*************************************/
/* Capabilities                                                              */
Capabilities::Capabilities(const char *title) {
	TiXmlDocument doc(title);
	string str;
	if (!doc.LoadFile()) {
		cerr << "erreur lors de la lecture du fichier XML" << endl;
		cerr << "error #" << doc.ErrorId() << " : " << doc.ErrorDesc() << endl;
		exit(1);
	}
	TiXmlNode *node = doc.FirstChild("capabilities");
	if (!node) { cerr << "erreur <capabilities>" << endl; return; };

	TiXmlElement *capaElem = node->FirstChildElement("capability");
	Capability *currentCapa=NULL;
	TiXmlElement *matrixElem;
	PointRel3D pos;
	while (capaElem) {
		str = capaElem->Attribute("name");
		currentCapa = new Capability(str);
		tabCapabilities.push_back(currentCapa);
		/* STATE */
		matrixElem = capaElem->FirstChildElement("states");
		if (!matrixElem) { cerr << "erreur <states>" << endl; return; };
		currentCapa->stateMatrix.fromString(matrixElem->GetText());
		/* THIS */
		matrixElem = capaElem->FirstChildElement("this");
		if (!matrixElem) { 
			cerr << "erreur <this>" << endl;
			return;
		} else {
			str = matrixElem->Attribute("pos");
			currentCapa->refPos.fromString(str);
		}
		/* MOTIONS */
		matrixElem = capaElem->FirstChildElement("motions");
		if (!matrixElem) { 
			cerr << "erreur <motions>" << endl;
			return;
		} else {
			TiXmlElement *motionElem = matrixElem->FirstChildElement("motion");
//			double first=1000,last=-1000;
			Motion *currentMotion=NULL;
			while (motionElem) {
				currentMotion = new Motion();
				if (motionElem->Attribute("obj")==NULL) {
					currentMotion->obj=0;
				} else {
					str = motionElem->Attribute("obj");
					currentMotion->obj=atoi(str.c_str());
				}
				cout << "obj = " << currentMotion->obj << endl;
				str = motionElem->Attribute("time");
				currentMotion->endTime = atof(str.c_str())+1.0;
				str = motionElem->Attribute("from");
				currentMotion->init.fromString(str,currentCapa->refPos);
				str = motionElem->Attribute("to");
				currentMotion->final.fromString(str,currentCapa->refPos);
				
				currentCapa->tabMotions.push_back(currentMotion);
				motionElem = motionElem->NextSiblingElement("motion");
			}

		}
		// create rotated clones
		char strNum[32];
		cout << *currentCapa << endl;
		for (int i=0; i<3; i++) {
#ifdef WIN32
			sprintf_s(strNum,"%s.%0d",currentCapa->name.c_str(),i+1);
#else
			sprintf(strNum,"%s.%0d",currentCapa->name.c_str(),i+1);
#endif
			tabCapabilities.push_back(new Capability(strNum,currentCapa,i+1));
			cout << *(tabCapabilities.back()) << endl;
		}
		capaElem = capaElem->NextSiblingElement("capability");
	}
}

Capabilities::~Capabilities() {
	vector<Capability*>::const_iterator it = tabCapabilities.begin();
	while (it!=tabCapabilities.end()) {
		delete (*it);
		it++;
	}
	tabCapabilities.clear();
}


/**************************************BPi************************************/
/* StateMatrix                                                               */
StateMatrix::StateMatrix() {
	memset(grid,0,sizeof(states)*27);
}

void StateMatrix::fromString(const char *str) {
	int ix=0,iy=2,iz=2,i=ix+iy*3+iz*9;
	const char *ptrStr=str;
	bool move;

	while (*ptrStr!=0) {
		move=true;
		switch (*ptrStr) {
		case '0' : 
			grid[i]=emptyState; 
			break;
		case '1' : 
			grid[i]=fullState; 
			break;
		case '2' : 
			grid[i]=anyState; 
			break;
		case '3' : 
			grid[i]=zeroToOneState; 
			break;
		case '4' : 
			grid[i]=oneToZeroState; 
			break;
		default : 
			move=false;
		}
		if (move) {
			ix++; 
			if (ix>2) { 
				ix=0; 
				iy--; 
				if (iy<0) { 
					iy=2; 
					iz--; 
				} 
			}
			i=ix+iy*3+iz*9;
		}
		ptrStr++;
	}
}

void StateMatrix::fromRotationZ(const StateMatrix& mat,int n) {
	int transf[4][27]={
		{0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26},
		{2,5,8,1,4,7,0,3,6,11,14,17,10,13,16,9,12,15,20,23,26,19,22,25,18,21,24},
		{8,7,6,5,4,3,2,1,0,17,16,15,14,13,12,11,10,9,26,25,24,23,22,21,20,19,18},
		{6,3,0,7,4,1,8,5,2,15,12,9,16,13,10,17,14,11,24,21,18,25,22,19,26,23,20}};
	int *matTransf = transf[n%4];
	for (int i=0; i<27; i++) {
		grid[i] = mat.grid[matTransf[i]];
	}

}

void StateMatrix::fromRotationY(const StateMatrix& mat,int n) {
	int transf[4][27]={
		{0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26},
		{2,11,20,5,14,23,8,17,26,1,10,19,4,13,22,7,16,25,0,9,18,3,12,21,6,15,24},
		{20,19,18,23,22,21,26,25,24,11,10,9,14,13,12,17,16,15,2,1,0,5,4,3,8,7,6},
		{18,9,0,21,12,3,24,15,6,19,10,1,22,13,4,25,16,7,20,11,2,23,14,5,26,17,8}};
	int *matTransf = transf[n%4];
	for (int i=0; i<27; i++) {
		grid[i] = mat.grid[matTransf[i]];
	}
}

states StateMatrix::get(const PointRel3D& pt) {
	return grid[pt.x+pt.y*3+pt.z*9+13];
}

void StateMatrix::set(const PointRel3D& pt,states v) {
	states *ptr = grid+pt.x+pt.y*3+pt.z*9+13;
	*ptr=v;
}

void  StateMatrix::lock(const PointRel3D& pt) {
	states *ptr = grid+pt.x+pt.y*3+pt.z*9+13;
	*ptr=states(*ptr+6);
}

// write StateMatrix values into a flux
ostream& operator<<(ostream& f,const StateMatrix &sm) { 
	f << sm.grid[24] << " " << sm.grid[25] << " " << sm.grid[26] << " | " << sm.grid[15] << " " << sm.grid[16] << " " << sm.grid[17] << " | " << sm.grid[6] << " " << sm.grid[7] << " " << sm.grid[8] << endl;
	f << sm.grid[21] << " " << sm.grid[22] << " " << sm.grid[23] << " | " << sm.grid[12] << " " << sm.grid[13] << " " << sm.grid[14] << " | " << sm.grid[3] << " " << sm.grid[4] << " " << sm.grid[5] << endl;
	f << sm.grid[18] << " " << sm.grid[19] << " " << sm.grid[20] << " | " << sm.grid[9] << " " << sm.grid[10] << " " << sm.grid[11] << " | " << sm.grid[0] << " " << sm.grid[1] << " " << sm.grid[2] << endl;
  return f;
}

int StateMatrix::distance(const StateMatrix &sm) {
	int n=27,dist=0;
	const states *ptr1=grid,*ptr2=sm.grid;
	states v1,v2;
	while (n--) {
		if (*ptr1!=anyState && *ptr2!=anyState) {
			v1 = *ptr1;
			if (v1==oneToZeroState) v1=fullState;
			if (v1==zeroToOneState) v1=emptyState;
			v2 = *ptr2;
			if (v2==oneToZeroState) v2=fullState;
			if (v2==zeroToOneState) v2=emptyState;
			if (v1+v2==1) dist++;
		}
		ptr1++;
		ptr2++;
	}
	return dist;
}

int StateMatrix::conditionalDistance(const StateMatrix &sm,PointRel3D &pos,states &value) {
	int n=27,dist=0,ind=-1;
	const states *ptr1=grid,*ptr2=sm.grid;
	pos.set(-10,-10,-10);
	bool goalReached=false;
	while (n--) {
		// il faut que l'on ait soit 3-3 ou 4-4
		if ((*ptr1==zeroToOneState && *ptr2==zeroToOneState)||
			(*ptr1==oneToZeroState && *ptr2==oneToZeroState)) goalReached=true;
		if (*ptr1==anyState || *ptr2==anyState || *ptr1==*ptr2 || 
			((*ptr1)==lockedEmptyState && (*ptr2)==emptyState) ||
			((*ptr1)==lockedFullState && (*ptr2)==fullState) ||
			((*ptr1)==fullState && (*ptr2)==oneToZeroState) ||
			((*ptr1)==emptyState && (*ptr2)==zeroToOneState)) {
		} else {
			// si il faut modifier une case lockee ! impossible
			if ((*ptr1)==lockedEmptyState || (*ptr1)==lockedFullState) return 1000;
			ind=26-n;
			pos.x = ind%3-1;
			pos.y = (ind%9)/3-1;
			pos.z = ind/9-1;
			value = (*ptr1);
			dist++;
		} 
		ptr1++;
		ptr2++;
	}
	return goalReached?dist:1000;
}

/**************************************BPi************************************/
/* PointRel3D                                                                */
PointRel3D::PointRel3D() {
	x=y=z=0;
}

PointRel3D::PointRel3D(short vx,short vy,short vz):x(vx),y(vy),z(vz) {
};
	
PointRel3D::PointRel3D(const string &str) {
	fromString(str);
}

void PointRel3D::fromString(const string &str) {
	int dx,dy,dz;
#ifdef WIN32
	sscanf_s(str.c_str(),"%d,%d,%d",&dx,&dy,&dz);
#else
	sscanf(str.c_str(),"%d,%d,%d",&dx,&dy,&dz);
#endif
	x=dx-1;
	y=dy-1;
	z=dz-1;
}

void PointRel3D::fromString(const string &str,const PointRel3D &ref) {
	int dx,dy,dz;
#ifdef WIN32
	sscanf_s(str.c_str(),"%d,%d,%d",&dx,&dy,&dz);
#else
	sscanf(str.c_str(),"%d,%d,%d",&dx,&dy,&dz);
#endif
	x=dx-ref.x-1;
	y=dy-ref.y-1;
	z=dz-ref.z-1;
}

// write PointRel3D values into a flux
ostream& operator<<(ostream& f,const PointRel3D &pt) { 
	f << "(" << pt.x << "," << pt.y << "," << pt.z << ")";
  return f;
}

PointRel3D PointRel3D::rotateY(int a) const {
	int tabMat[4][4]={
		{1,0,0,1},
		{0,1,-1,0},
		{-1,0,0,-1},
		{0,-1,1,0}};
	PointRel3D res;
	int *mat = tabMat[a%4];
	res.x = x*mat[0]+z*mat[1];
	res.y = y;
	res.z = x*mat[2]+z*mat[3];
	return res;
}

PointRel3D PointRel3D::rotateZ(int a) const {
	int tabMat[4][4]={
		{1,0,0,1},
		{0,1,-1,0},
		{-1,0,0,-1},
		{0,-1,1,0}};
	PointRel3D res;
	int *mat = tabMat[a%4];
	res.x = x*mat[0]+y*mat[1];
	res.y = x*mat[2]+y*mat[3];
	res.z = z;
	return res;
}

void PointRel3D::set(short vx,short vy,short vz) {
	x=vx;
	y=vy;
	z=vz;
}

bool PointRel3D::isInBoundingBox(short x0,short x1,short y0,short y1,short z0,short z1) {
	return (x>=x0 && x<=x1 && y>=y0 && y<=y1 && z>=z0 && z<=z1);
}

bool operator <=(const PointRel3D p1,const PointRel3D p2) {
	if (p1.x<p2.x) return true;
	if (p1.x>p2.x) return false;
	if (p1.y<p2.y) return true;
	if (p1.y>p2.y) return false;
	return (p1.z<=p2.z);
}

bool operator >(const PointRel3D p1,const PointRel3D p2) {
	if (p1.x>p2.x) return true;
	if (p1.x<p2.x) return false;
	if (p1.y>p2.y) return true;
	if (p1.y<p2.y) return false;
	return (p1.z>p2.z);
}

bool operator !=(const PointRel3D p1,const PointRel3D p2) {
	return (p1.x!=p2.x || p1.y!=p2.y || p1.z!=p2.z);
}

/**************************************BPi************************************/
/* Motion                                                                    */
Motion::Motion() {
}

Motion::Motion(const Motion *src) {
	obj = src->obj;
	endTime = src->endTime;
	init = src->init;
	final = src->final;
}
