#include <math.h>
#include "block.bbh"
#include "clock.bbh"

#define BLINK_AQUA	1

threadtype typedef float bMatrix[6][6];

threadvar int maxIterations ; // max number of iterations
threaddef #define _E	100.0
threaddef #define _L	40.0
threaddef #define _a	40.0
threaddef #define _A	1600.0
threaddef #define _I	213333.333333333
threaddef #define _Iz	213333.333333333
threaddef #define _Iy	213333.333333333
threaddef #define _nu	0.3
threaddef #define _J	360000.0

threaddef #define mass	0.06103
threaddef #define grav	9.81
threaddef #define beta	2.0/3.0

threaddef #define Omega	2.0/3.0
threaddef #define Mu	0.1
threaddef #define Eps	1.0E-8
threaddef #define Gamma	1.0E-6

threaddef #define K11(i,j,k)((const float[6][6][6]) {{{12.*_E*_Iy/(_L*_L*_L), 0, 0, 0, 6.*_E*_Iy/(_L*_L), 0},{0, 12.*_E*_Iz/(_L*_L*_L), 0, -6*_E*_Iz/(_L*_L), 0, 0},{0, 0, _A*_E/_L, 0, 0, 0},{0, -6*_E*_Iz/(_L*_L), 0 , 4*_E*_Iz/_L, 0, 0},{6*_E*_Iy/(_L*_L), 0, 0, 0, 4*_E*_Iy/_L, 0},{0, 0, 0, 0, 0, _E*_J/(2*_L*(1*_nu))}},{{12.*_E*_Iy/(_L*_L*_L), 0, 0, 0, -6.*_E*_Iy/(_L*_L), 0},{0, 12.*_E*_Iz/(_L*_L*_L), 0, 6*_E*_Iz/(_L*_L), 0, 0},{0, 0, _A*_E/_L, 0, 0, 0},{0, 6*_E*_Iz/(_L*_L), 0 , 4*_E*_Iz/_L, 0, 0},{-6*_E*_Iy/(_L*_L), 0, 0, 0, 4*_E*_Iy/_L, 0},{0, 0, 0, 0, 0, _E*_J/(2*_L*(1*_nu))}},{{_A*_E/_L, 0, 0, 0, 0, 0},{0, 12*_E*_Iz/(_L*_L*_L),  0, 0, 0, -6*_E*_Iz/(_L*_L)},{0, 0, 12*_E*_Iy/(_L*_L*_L), 0, 6*_E*_Iy/(_L*_L), 0},{0, 0, 0, _E*_J/(2*_L*(1+_nu)), 0, 0},{0, 0, 6*_E*_Iy/(_L*_L), 0, 4*_E*_Iy/_L, 0},{0, -6*_E*_Iz/(_L*_L), 0, 0, 0, 4*_E*_Iz/_L}},{{_A*_E/_L, 0, 0, 0, 0, 0},{0, 12*_E*_Iz/(_L*_L*_L),  0, 0, 0, 6*_E*_Iz/(_L*_L)},{0, 0, 12*_E*_Iy/(_L*_L*_L), 0, -6*_E*_Iy/(_L*_L), 0},{0, 0, 0, _E*_J/(2*_L*(1+_nu)), 0, 0},{0, 0, -6*_E*_Iy/(_L*_L), 0, 4*_E*_Iy/_L, 0},{0, 6*_E*_Iz/(_L*_L), 0, 0, 0, 4*_E*_Iz/_L}},{	{12.*_E*_Iz/(_L*_L*_L), 0, 0, 0, 0, 6*_E*_Iz/(_L*_L)},{0, _A*_E/_L, 0, 0, 0, 0},{0, 0, 12*_E*_Iy/(_L*_L*_L), -6*_E*_Iy/(_L*_L), 0, 0},{0, 0, -6*_E*_Iy/(_L*_L), 4*_E*_Iy/_L, 0, 0},{0, 0, 0, 0, _E*_J/(2*_L*(1+_nu)), 0},{6*_E*_Iz/(_L*_L), 0, 0, 0, 0, 4*_E*_Iz/_L}},{	{12.*_E*_Iz/(_L*_L*_L), 0, 0, 0, 0, -6*_E*_Iz/(_L*_L)},{0, _A*_E/_L, 0, 0, 0, 0},{0, 0, 12*_E*_Iy/(_L*_L*_L), 6*_E*_Iy/(_L*_L), 0, 0},{0, 0, 6*_E*_Iy/(_L*_L), 4*_E*_Iy/_L, 0, 0},{0, 0, 0, 0, _E*_J/(2*_L*(1+_nu)), 0},{-6*_E*_Iz/(_L*_L), 0, 0, 0, 0, 4*_E*_Iz/_L}}}[i][j][k])

threaddef #define K12(i,j,k)((const float[6][6][6]) {{{-12.*_E*_Iy/(_L*_L*_L), 0, 0, 0, 6*_E*_Iy/(_L*_L),0},{0,-12.*_E*_Iz/(_L*_L*_L), 0, -6*_E*_Iz/(_L*_L), 0, 0},{0, 0, -_A*_E/_L, 0, 0, 0},{0, 6*_E*_Iz/(_L*_L), 0, 2*_E*_Iz/_L, 0, 0},{-6*_E*_Iy/(_L*_L), 0 ,0, 0, 2*_E*_Iy/_L, 0},{0, 0, 0, 0, 0, -_E*_J/(2*_L*(1+_nu))}},{{-12.*_E*_Iy/(_L*_L*_L), 0, 0, 0, -6*_E*_Iy/(_L*_L),0},	{0,-12.*_E*_Iz/(_L*_L*_L), 0, 6*_E*_Iz/(_L*_L), 0, 0},{0, 0, -_A*_E/_L, 0, 0, 0},{0, -6*_E*_Iz/(_L*_L), 0, 2*_E*_Iz/_L, 0, 0},{6*_E*_Iy/(_L*_L), 0 ,0, 0, 2*_E*_Iy/_L, 0},{0, 0, 0, 0, 0, -_E*_J/(2*_L*(1+_nu))}},{{-_A*_E/_L, 0, 0, 0, 0, 0},	{0, -12*_E*_Iz/(_L*_L*_L),  0, 0, 0, -6*_E*_Iz/(_L*_L)},{0, 0, -12*_E*_Iy/(_L*_L*_L), 0, 6*_E*_Iy/(_L*_L), 0},{0, 0, 0, -_E*_J/(2*_L*(1+_nu)), 0, 0},{0, 0, -6*_E*_Iy/(_L*_L), 0, 2*_E*_Iy/_L, 0},{0, 6*_E*_Iz/(_L*_L), 0, 0, 0, 2*_E*_Iz/_L}},{{-_A*_E/_L, 0, 0, 0, 0, 0},{0, -12*_E*_Iz/(_L*_L*_L),  0, 0, 0, 6*_E*_Iz/(_L*_L)},{0, 0, -12*_E*_Iy/(_L*_L*_L), 0, -6*_E*_Iy/(_L*_L), 0},{0, 0, 0, -_E*_J/(2*_L*(1+_nu)), 0, 0},{0, 0, 6*_E*_Iy/(_L*_L), 0, 2*_E*_Iy/_L, 0},{0, -6*_E*_Iz/(_L*_L), 0, 0, 0, 2*_E*_Iz/_L}},{{-12.*_E*_Iz/(_L*_L*_L), 0, 0, 0, 0, 6*_E*_Iz/(_L*_L)},{0, -_A*_E/_L, 0, 0, 0, 0},{0, 0, -12*_E*_Iy/(_L*_L*_L), -6*_E*_Iy/(_L*_L), 0, 0},{0, 0, 6*_E*_Iy/(_L*_L), 2*_E*_Iy/_L, 0, 0},{0, 0, 0, 0, -_E*_J/(2*_L*(1+_nu)), 0},{-6*_E*_Iz/(_L*_L), 0, 0, 0, 0, 2*_E*_Iz/_L}},{{-12.*_E*_Iz/(_L*_L*_L), 0, 0, 0, 0, -6*_E*_Iz/(_L*_L)},{0, -_A*_E/_L, 0, 0, 0, 0},{0, 0, -12*_E*_Iy/(_L*_L*_L), 6*_E*_Iy/(_L*_L), 0, 0},{0, 0, -6*_E*_Iy/(_L*_L), 2*_E*_Iy/_L, 0, 0},{0, 0, 0, 0, -_E*_J/(2*_L*(1+_nu)), 0},{6*_E*_Iz/(_L*_L), 0, 0, 0, 0, 2*_E*_Iz/_L}}}[i][j][k])

threaddef #define Rot(i,j,k)((const float[6][6][6]) {{{0,0,1,0,0,0},{0,1,0,0,0,0},{-1,0,0,0,0,0},{0,0,0,0,0,1},{0,0,0,0,1,0},{0,0,0,-1,0,0}},{{0,0,-1,0,0,0},{0,1,0,0,0,0},{1,0,0,0,0,0},{0,0,0,0,0,-1},{0,0,0,0,1,0},{0,0,0,1,0,0}},{{-1,0,0,0,0,0},{0,-1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,-1,0,0},{0,0,0,0,-1,0},{0,0,0,0,0,1}},{{1,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}},{{0,-1,0,0,0,0},{1,0,0,0,0,0},{0,0,1,0,0,0},{0,0,0,0,-1,0},{0,0,0,1,0,0},{0,0,0,0,0,1}},{{0,1,0,0,0,0},{-1,0,0,0,0,0},{0,0,1,0,0,0},{0,0,0,0,1,0},{0,0,0,-1,0,0},{0,0,0,0,0,1}}}[i][j][k])

threaddef #define Tfr(i,j,Kxx,Kxy,Kxz,Kyx,Kyy,Kyz)((const float[6][6]) {{{Kxx,Kxy,Kxz,0  ,0  ,0  },{Kyx,Kyy,Kyz,0  ,0  ,0  },{0  ,0  ,1  ,0  ,0  ,0  },{0  ,0  ,0  ,1  ,0  ,0  },{0  ,0  ,0  ,0  ,1  ,0  },{0  ,0  ,0  ,0  ,0  ,1  }}[i][j]);

threaddef #define Tmx(i,j,Kxz)((const float[6][6]) {{{1  ,0  ,0  ,0  ,0  ,0  },{0  ,1  ,0  ,0  ,0  ,0  },{0  ,0  ,1  ,0  ,0  ,0  },{0  ,0  ,Kxz,0  ,0  ,0  },{0  ,0  ,0  ,0  ,1  ,0  },{0  ,0  ,0  ,0  ,0  ,1  }}[i][j]);

threaddef #define Tmy(i,j,Kxz)((const float[6][6]) {{{1  ,0  ,0  ,0  ,0  ,0  },{0  ,1  ,0  ,0  ,0  ,0  },{0  ,0  ,1  ,0  ,0  ,0  },{0  ,0  ,0  ,1  ,0  ,0  },{0  ,0  ,Kxz,0  ,0  ,0  },{0  ,0  ,0  ,0  ,0  ,1  }}[i][j]);

threaddef #define Tmz(i,j,Kxz)((const float[6][6]) {{{1  ,0  ,0  ,0  ,0  ,0  },{0  ,1  ,0  ,0  ,0  ,0  },{0  ,0  ,1  ,0  ,0  ,0  },{0  ,0  ,0  ,1  ,0  ,0  },{0  ,0  ,0  ,0  ,1  ,0  },{0  ,0  ,Kxz,0  ,0  ,0  }}[i][j]);

threadvar byte virtualPosition[5][3];//={{3,0,2},{4,0,2},{5,0,2},{6,0,2},{7,0,2}};
threadvar byte nbStages=5;
threadvar byte currentStage=1;

threadvar byte neighbors[6][2];// Neighbors 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1 ///// second row  if tehre is a message with du

threadvar byte isSupport = 0;
threadvar byte isFixed = 0;
threadvar float orient[6];
threadvar int curIteration = 0; // current iteration

//matrix for visualization pf calculated forces and moments
threadvar bMatrix vizTable;

threadvar bMatrix uq; //vector u from -1 step neighbors

threadvar float dup[6]; // vector u from previus step
threadvar float du[6];
threadvar float fp[6];
threadvar float Fp[6];

threadvar byte position[3];
threadvar byte xplusBorder=SOUTH;
threadvar byte yplusBorder=EAST;
threadvar byte zplusBorder=UP;
threadvar byte currentBBColor[3];

threaddef #define NO_PARENT	6

threadvar uint8_t tree_parent=NO_PARENT;
threadvar uint8_t nbreWaitedAnswers=0;
threadvar uint8_t blinkMode=0;
threadvar uint8_t started=0;

/*****************************/
/** prototypes of functions **/

byte sendCoordChunk(PRef p);
byte coordMessageHandler(void);
byte sendBackChunk(PRef p);
byte backMessageHandler(void);
Chunk* getFreeUserChunk(void);
void AckCallback(void);
void AckCoordCallback(void);

void ForcesPredictionIPPTCode_startup(void);
void ForcesPredictionIPPTCode_setNeighbors(void);
void ForcesPredictionIPPTCode_setVirtualNeighbors(void);
void ForcesPredictionIPPTCode_visualization(void);
void ForcesPredictionIPPTCode_computeNeighborDU(int i);
void ForcesPredictionIPPTCode_computeDU(void);
byte ForcesPredictionIPPTCode_isFixed(void);
byte ForcesPredictionIPPTCode_isSupport(void);
//void printVector(vector6 &vec, int row=3,string desc="");
//void printMatrix(vector< vector6 > &matrix, int row=vectorSize, int col=vectorSize, string desc="");
void ForcesPredictionIPPTCode_clearNeighborsMessage(void); //function to clear messages when calculated u

void ForcesPredictionIPPTCode_createRevD(bMatrix mat, bMatrix res);
void ForcesPredictionIPPTCode_createK11(int i,bMatrix mat);
void ForcesPredictionIPPTCode_createK12(int i,bMatrix mat);
void ForcesPredictionIPPTCode_createRot(int i,bMatrix mat);
void ForcesPredictionIPPTCode_createR(bMatrix mat, bMatrix res);
void ForcesPredictionIPPTCode_createD(bMatrix mat, bMatrix res);

void ForcesPredictionIPPTCode_ProcSendDuFunc(float msgData[6], PRef sender, byte dec);
void ForcesPredictionIPPTCode_sendMessageToAllNeighbors(float v[6]);
void ForcesPredictionIPPTCode_CheckNeighbors(void);

void ForcesPredictionIPPTCode_createTfr(float Txx, float Txy, float Txz,float Tyx, float Tyy, float Tyz, bMatrix res);
void ForcesPredictionIPPTCode_createTmx(float Txz, bMatrix res);
void ForcesPredictionIPPTCode_createTmy(float Txz, bMatrix res);
void ForcesPredictionIPPTCode_createTmz(float Txz, bMatrix res);

byte sendVectorPartToPort(PRef p,float v[6],byte part);
byte vectorDataMessageHandler(void);
//void parseUserElements(TiXmlDocument* config);
//void parseUserBlockElements(TiXmlElement* config);

void initMatrix(bMatrix mat);
void identityMatrix(bMatrix mat);
void initVector(float vec[6]);
void copyVector(float src[6],float res[6]);
void copyVectorToMatrix(float res[6],bMatrix mat,byte col);
void copyMatrixToVector(bMatrix mat,byte col,float res[6]);
void multVectConst(float vec[6],float scal,float res[6]);
void addVectors(float *vec1,float *vec2,float *res);
void addVectorsToMatrix(float *vec,bMatrix mat,byte col);
void subVectors(float *vec1,float *vec2,float *res);
void addMatrices(bMatrix mat1,bMatrix mat2,bMatrix res);
void multMatrixConst(bMatrix mat1,float mat2,bMatrix res);
void multMatrixVector(bMatrix mat1,float vec[6],float res[6]);
void multMatrices(bMatrix mat1,bMatrix mat2,bMatrix res);
void printVector(char *str,float vec[6]);
void printMatrix(char *titre,bMatrix mat);

float max(float a,float b) { return (a>b)?a:b; };
float min(float a,float b) { return (a<b)?a:b; };
float sign(float x) { return (x>=0)?1.0:-1.0; };

threaddef #define MYCHUNKS 24
threadextern Chunk* thisChunk;
threadvar Chunk myChunks[MYCHUNKS];

void myMain(void) {
	started=0;
	setLED(0,0,0,0);
	position[0] = 0;
	position[1] = 0;
	position[2] = 0;
	currentBBColor[0]=0;
	currentBBColor[1]=0;
	currentBBColor[2]=0;
	// Problems with BBsim, must reinit !
	tree_parent=NO_PARENT;
	nbreWaitedAnswers=0;
	blinkMode=0;
	xplusBorder=SOUTH;
	yplusBorder=EAST;
	zplusBorder=UP;
	isSupport=0;
	isFixed=0;
	curIteration = 0;
	maxIterations = 15000;
	nbStages=5;
	currentStage=3;
	//={{3,0,2},{4,0,2},{5,0,2},{6,0,2}};
	virtualPosition[0][0]=3;virtualPosition[0][1]=0;virtualPosition[0][2]=2;
	virtualPosition[1][0]=4;virtualPosition[1][1]=0;virtualPosition[1][2]=2;
	virtualPosition[2][0]=5;virtualPosition[2][1]=0;virtualPosition[2][2]=2;
	virtualPosition[3][0]=6;virtualPosition[3][1]=0;virtualPosition[3][2]=2;

	delayMS(500);
	while(getNeighborCount()==0) {
		delayMS(6);
	}

	initMatrix(vizTable);
	initVector(dup);
	initMatrix(uq);
	initVector(du);
	initVector(fp);
	initVector(Fp);
	// vector<float> orient={0,0,-1,0,0,0};
	initVector(orient);
	orient[2]=-1.0f;

	//cheking neighbors and adding them to a list
	ForcesPredictionIPPTCode_setNeighbors();

	//setting of the mass force vector
	//Fp=orient*grav*mass;
	multVectConst(orient,grav*mass,Fp);

#ifdef CLOCK_SYNC
	setLED(64,64,0,64);
	while (!isSynchronized()) {
		delayMS(6);
	}
#endif

	setLED(0,0,0,0);
#ifdef CLOCK_SYNC
	if (isTimeLeader()) {
#else
	if (getGUID()==1) {
#endif
		delayMS(1000);
		setColor(RED);
		position[0]=127;
		position[1]=127;
		position[2]=127;
		for (byte i=0; i<6; i++) {
			if (neighbors[i][0]) {
				sendCoordChunk(i);
				nbreWaitedAnswers++;
			}
		}
	}

	while (1) {
		setLED(currentBBColor[0],currentBBColor[1],currentBBColor[2],255);
		delayMS(100);
		if (blinkMode) {
		    delayMS(400);
            setColor(WHITE);
			delayMS(500);
		}
	}
}

/*
void neighborChangeDetect() {

}
*/

void userRegistration(void) {
	registerHandler(SYSTEM_MAIN, (GenericHandler)&myMain);
	//registerHandler(EVENT_NEIGHBOR_CHANGE, (GenericHandler)&neighborChangeDetect);
}

// Send a chunk with a specific message type
byte sendCoordChunk(PRef p) {
	Chunk *c = getFreeUserChunk();
	if (c != NULL) {
		c->data[0]=position[0];
		c->data[1]=position[1];
		c->data[2]=position[2];
		c->data[3]=0;
		if (p==xplusBorder) {
			c->data[0]++;
			c->data[3]=1; // axe X
		} else if (p==5-xplusBorder) {
			c->data[0]--;
			c->data[3]=2; // axe -X
		} else if (p==yplusBorder) {
			c->data[1]++;
			c->data[3]=3; // axe Y
		} else if (p==5-yplusBorder) {
			c->data[1]--;
			c->data[3]=5; // axe -Y
		} else if (p==zplusBorder) {
			c->data[2]++;
			c->data[3]=6; // axe Z
		} else if (p==5-zplusBorder) {
			c->data[2]--;
			c->data[3]=7; // axe -Z
		}
		if (sendMessageToPort(c, p, c->data, 4, coordMessageHandler, AckCoordCallback) == 0) {
			freeChunk(c);
			return 0;
		}
	}
	return 1;
}

byte coordMessageHandler(void) {
	if (thisChunk == NULL) return 0;

	byte sender = faceNum(thisChunk);
	if (tree_parent==NO_PARENT) {
		nbreWaitedAnswers=0;
		tree_parent=sender;
		position[0] = thisChunk->data[0];
		position[1] = thisChunk->data[1];
		position[2] = thisChunk->data[2];
		for (byte i=0; i<6; i++) {
			if (neighbors[i][0] && i!=sender) {
				sendCoordChunk(i);
				nbreWaitedAnswers++;
			}
		}
		if (nbreWaitedAnswers==0) {
			sendBackChunk(tree_parent);
			if (!started) ForcesPredictionIPPTCode_startup();
		}
	} else {
		sendBackChunk(sender);
	}
	return 1;
}

// Send a chunk with a specific message type
byte sendBackChunk(PRef p) {
  Chunk *c = getFreeUserChunk();

  if (c != NULL) {
    if (sendMessageToPort(c, p, c->data, 0, backMessageHandler, AckCoordCallback) == 0) {
      freeChunk(c);
      return 0;
    }
  }
  return 1;
}

byte backMessageHandler(void) {
	if (thisChunk == NULL) return 0;
	//byte sender = faceNum(thisChunk);

	nbreWaitedAnswers--;
	if (nbreWaitedAnswers==0) {
		if (tree_parent!=NO_PARENT) {
			sendBackChunk(tree_parent);
		} else {
			//blinkMode=1;
		}
		if (!started) ForcesPredictionIPPTCode_startup();
	}
	return 1;
}

// find a useable chunk
Chunk* getFreeUserChunk(void) {
	Chunk* c;
	int i;

	for(i=0; i<MYCHUNKS; i++) {
		c = &(myChunks[i]);
		if( !chunkInUse(c) ) {
			return c;
		}
	}
	return NULL;
}

void ForcesPredictionIPPTCode_contactStiffnessMatrix(float vdup[6],bMatrix K11d) {
	// stiffness matrix for DOWN direction
	ForcesPredictionIPPTCode_createK11(1,K11d);
    bMatrix TfrB, TmxB, TmyB, TmzB;
    float Fd[6];// = K11d*dup;
	multMatrixVector(K11d,vdup,Fd);
	float fx=Fd[0], fy=Fd[1], fz=Fd[2], mx=Fd[3], my=Fd[4], mz=Fd[5]; // elastic predictor

    if(fz<0) { // contact
        float mmax = -fz*_a/2.0;
        float fmax = -Mu*fz;

        // condition for frictional sliding
        if(sqrt(fx*fx+fy*fy)<fmax) { // frictional stick state
			identityMatrix(TfrB);
		} else { // frictionaal slip state
			if(sqrt(fx*fx+fy*fy)<Eps) { // near zero tangential force -> Mu is probably very low -> stiffness matrix for sliding = 0
				ForcesPredictionIPPTCode_createTfr(0,0,0,0,0,0,TfrB);
			} else { // frictional sliding (radial return on the Coulomb friction cone
				ForcesPredictionIPPTCode_createTfr(
					-fy*fy*fz*Mu/pow(fx*fx+fy*fy,3/2),
					fx*fy*fz*Mu/pow(fx*fx+fy*fy,3/2),
					-fx*Mu/pow(fx*fx+fy*fy,1/2),
					fx*fy*fz*Mu/pow(fx*fx+fy*fy,3/2),
					-fx*fx*fz*Mu/pow(fx*fx+fy*fy,3/2),
					-fy*Mu/pow(fx*fx+fy*fy,1/2),TfrB);
				}
			}

        // condition for x-tilting (over the y-directed edge (front or back))
        if(fabs(mx) < mmax) { // stable bending
			identityMatrix(TmxB);
		} else { // unstable bending (tilting over the y-directed edge)
			if(fabs(mx)<Eps) { // near-zero torque -> fz is near zero -> stiffness matrix for x-bending will be zero
				ForcesPredictionIPPTCode_createTmx(0,TmxB);
			} else { // tilting occurs
				ForcesPredictionIPPTCode_createTmx(-sign(mx)*_a/2,TmxB);
			}
		}

        // condition for y-tilting (over the x-directed edge (left or right))
		if(fabs(my) < mmax) { // stable bending
			identityMatrix(TmyB);
		} else { // unstable bending (tilting over the y-directed edge)
			if(fabs(my)<Eps) { // near-zero torque -> fz is near zero -> stiffness matrix for x-bending will be zero
				ForcesPredictionIPPTCode_createTmy(0,TmyB);
			} else { // tilting occurs
				ForcesPredictionIPPTCode_createTmy(-sign(my)*_a/2,TmyB);
			}
		}

        // condition for x-tilting (over the y-directed edge (front or back))
		if(fabs(mz) < mmax) { // stable bending
			identityMatrix(TmzB);
		} else { // unstable bending (tilting over the y-directed edge)
            if(fabs(mz)<Eps) { // near-zero torque -> fz is near zero -> stiffness matrix for x-bending will be zero
                ForcesPredictionIPPTCode_createTmz(0,TmzB);
            } else { // tilting occurs
                ForcesPredictionIPPTCode_createTmz(-sign(mz)*Mu,TmzB);
            }
        }
        //K11d=TfrB*TmxB*TmyB*TmzB*K11d;
		multMatrices(TmzB,K11d,K11d);
		multMatrices(TmyB,K11d,K11d);
		multMatrices(TmxB,K11d,K11d);
		multMatrices(TfrB,K11d,K11d);
    } else { // separation
        //K11d=Gamma*K11d;
		multMatrixConst(K11d,Gamma,K11d);
    }
//    return K11d;
}

void ForcesPredictionIPPTCode_computeNeighborDU(int i) {
	int di=1-2*(i%2);
	bMatrix sumK11;
	ForcesPredictionIPPTCode_createK11(i+di,sumK11);
	bMatrix mtmp;
	float Fpq[6];// = createK12(i+di)*dup;
	ForcesPredictionIPPTCode_createK12(i+di,mtmp);
	multMatrixVector(mtmp,dup,Fpq);

	initMatrix(mtmp);
	float vtmp[6],vtmp2[6];

	if(isSupport && i>1) { // enforce the unilateral contact conditions with the support, located below the module only if the neighbor is on left, right, back or front
        //sumK11=sumK11+contactStiffnessMatrix(uq[i]);
		ForcesPredictionIPPTCode_contactStiffnessMatrix(uq[i],mtmp);
		addMatrices(sumK11,mtmp,sumK11);
	}
    // uq[i] = RevD(sumK11)*beta*(Fp-createR(sumK11)*uq[i]-Fpq)+(uq[i]*(1-beta));
	multVectConst(uq[i],1.0-beta,vtmp);
	ForcesPredictionIPPTCode_createR(sumK11,mtmp);
	multMatrixVector(mtmp,uq[i],vtmp2);
	subVectors(Fp,vtmp2,vtmp2);
	subVectors(vtmp2,Fpq,vtmp2);
	ForcesPredictionIPPTCode_createRevD(sumK11,mtmp);
	multMatrixConst(mtmp,beta,mtmp);
	multMatrixVector(mtmp,vtmp2,vtmp2);
	addVectors(vtmp2,vtmp,uq[i]);

    //printVector(uq[i],6,"neighbor vector uq["+ to_string(i) +"] of du module id= " + to_string(module->blockId) + ", iteration "+ to_string(curIteration));
}

void ForcesPredictionIPPTCode_computeDU() {
	//temporary Matrixes
	bMatrix sumK11;
	initMatrix(sumK11);
	float Fpq[6];
	initVector(Fpq);

	bMatrix mtmp;
	float vtmp[6];
	float vtmp2[6];

	if(!isFixed) {
		//checking neighbors and creating K11 and K12 matrixes
		for(size_t i=0;i<6;i++){
			//printf("neighbor %d(%d)\n",(int)i,(int)neighbors[i][1]);
			if(neighbors[i][1]!=0) { // messages received or a virtual module present
				if(neighbors[i][1]==3) ForcesPredictionIPPTCode_computeNeighborDU(i); // compute uq[i] for a virtual module
				//sumK11=sumK11+createK11(i);
				ForcesPredictionIPPTCode_createK11(i,mtmp);
				addMatrices(sumK11,mtmp,sumK11);
				//Fpq = Fpq+(createK12(i)*uq[i]);
				ForcesPredictionIPPTCode_createK12(i,mtmp);
				multMatrixVector(mtmp,uq[i],vtmp);
				addVectors(Fpq,vtmp,Fpq);
			}
		}
		if(isSupport) { // enforce the unilateral contact conditions with the support, located below the module
            // sumK11=sumK11+contactStiffnessMatrix(dup);
			ForcesPredictionIPPTCode_contactStiffnessMatrix(dup,mtmp);
			addMatrices(sumK11,mtmp,sumK11);
		}

        // du = RevD(sumK11)*beta*(Fp-createR(sumK11)*dup-Fpq)+(dup*(1-beta));
		multVectConst(dup,1.0-beta,vtmp);
		ForcesPredictionIPPTCode_createR(sumK11,mtmp);
		multMatrixVector(mtmp,dup,vtmp2);
		subVectors(Fp,vtmp2,vtmp2);
		subVectors(vtmp2,Fpq,vtmp2);
		ForcesPredictionIPPTCode_createRevD(sumK11,mtmp);
		multMatrixConst(mtmp,beta,mtmp);
		multMatrixVector(mtmp,vtmp2,vtmp2);
		addVectors(vtmp2,vtmp,du);

		char str[64];
		sprintf(str,"vector _du module id= %d, iteration %d",(int)getGUID(),curIteration);
		printVector(str,du);
		//printVector(du,6,"vector du module id= " + to_string(module->blockId) + ", iteration "+ to_string(curIteration));

	}	else { //end isFixed
        // du = du*0.;
		initVector(du);
	}

	//sending message to neighbors with du
//	OUTPUT << "size=" << du.size() << endl;
	//sendMessageToAllNeighbors("DU_MSG",new MessageOf<vector<double>>(DU_MSG,du),messageDelay,messageDelayError,0);
	ForcesPredictionIPPTCode_sendMessageToAllNeighbors(du);
	//clearing info about du from neighbors
	ForcesPredictionIPPTCode_clearNeighborsMessage();

}

void ForcesPredictionIPPTCode_setNeighbors() {
	//set 0 for all empty neighbors
	printf("set %d:",(int)getGUID());
	size_t n=0;
	do {
		n=0;
		for(size_t i=0;i<6;i++) {
			if (thisNeighborhood.n[i]!=VACANT) {
				neighbors[i][0] = 1;
				neighbors[i][1] = 2;
				n++;
			} else {
				neighbors[i][0] = 0;
				neighbors[i][1] = 0;
			}
printf("%d,",neighbors[i][0]);
		}
	} while (n!=getNeighborCount());
printf("\n");
//	ForcesPredictionIPPTCode_CheckNeighbors();
/*
	neighbors[2][0] = thisNeighborhood.n[5-xplusBorder];
	neighbors[3][0] = thisNeighborhood.n[xplusBorder];
	neighbors[4][0] = thisNeighborhood.n[5-yplusBorder];
	neighbors[5][0] = thisNeighborhood.n[yplusBorder];
	neighbors[0][0] = thisNeighborhood.n[zplusBorder];
	neighbors[1][0] = thisNeighborhood.n[5-zplusBorder];*/
}

void ForcesPredictionIPPTCode_setVirtualNeighbors() {
	int dir[6][3];
	dir[xplusBorder][0]=1; dir[xplusBorder][1]=0; dir[xplusBorder][2]=0;
	dir[5-xplusBorder][0]=-1; dir[5-xplusBorder][1]=0; dir[5-xplusBorder][2]=0;
	dir[yplusBorder][0]=0; dir[yplusBorder][1]=1; dir[yplusBorder][2]=0;
	dir[5-yplusBorder][0]=0; dir[5-yplusBorder][1]=-1; dir[5-yplusBorder][2]=0;
	dir[zplusBorder][0]=0; dir[zplusBorder][1]=0; dir[zplusBorder][2]=1;
	dir[5-zplusBorder][0]=0; dir[5-zplusBorder][1]=0; dir[5-zplusBorder][2]=-1;

	for(size_t i=0;i<6;i++) {
//		printf("%d:(%d == %d) (%d == %d) (%d == %d)\n",(int)getGUID(),position[0]+dir[i][0],127+virtualPosition[currentStage][0], position[1]+dir[i][1],127+virtualPosition[currentStage][1], position[2]+dir[i][2],127+virtualPosition[currentStage][2]);
		if (position[0]+dir[i][0]==(127+virtualPosition[currentStage][0]) &&
			position[1]+dir[i][1]==(127+virtualPosition[currentStage][1]) &&
			position[2]+dir[i][2]==(127+virtualPosition[currentStage][2])) {
				neighbors[i][1] = 3;
				blinkMode=1;
		} else {
			//neighbors[i][1] = 2;
			//blinkMode=0;
		}
	}
}

void ForcesPredictionIPPTCode_startup() {
	started=1;
	printf("Startup %d:(%d,%d,%d)\n",(int)getGUID(),position[0],position[1],position[2]);
	//check is module fixed
	if(isFixed) {
	//setColor(RED);
		currentBBColor[0]=255;
		currentBBColor[1]=0;
		currentBBColor[2]=0;
    }
	//check is module support
	isSupport=ForcesPredictionIPPTCode_isSupport();
	isFixed=ForcesPredictionIPPTCode_isFixed();
	// define who is fixed ?
	// isFixed ?

	//first step - calculate DU and sends to neighbor only in first
	if(curIteration == 0){
		ForcesPredictionIPPTCode_setVirtualNeighbors();
		ForcesPredictionIPPTCode_computeDU();
		curIteration++;
	}
}

byte ForcesPredictionIPPTCode_isSupport() {
	if(position[2]==127){
		// setColor(WHITE);
		currentBBColor[0]=255;
		currentBBColor[1]=255;
		currentBBColor[2]=255;
		return 1;
	}
	return 0;
}

byte ForcesPredictionIPPTCode_isFixed() {
	if(getGUID()>100){
		// setColor(WHITE);
		currentBBColor[0]=255;
		currentBBColor[1]=128;
		currentBBColor[2]=255;
		return 1;
	}
	return 0;
}

void ForcesPredictionIPPTCode_clearNeighborsMessage() {
	printf("%d rec clear\n",(int)getGUID());
	for(size_t i=0;i<6;i++) {
		if(neighbors[i][1]==2) {
			neighbors[i][1]=0;
		}
	}
}

void ForcesPredictionIPPTCode_sendMessageToAllNeighbors(float v[6]) {
	printf("%d: Sends (%f,%f,%f,%f,%f,%f) to ",(int)getGUID(),v[0],v[1],v[2],v[3],v[4],v[5]);
	for(size_t i=0;i<6;i++) {
		if (neighbors[i][0]) {
			printf("%d(%d),",(int)thisNeighborhood.n[i],(int)i);
			sendVectorPartToPort(i,v,0);
			sendVectorPartToPort(i,v,3);
		}
	}
	printf("\n");
}

byte sendVectorPartToPort(PRef p,float v[6],byte part) {
	Chunk *c = getFreeUserChunk();
	if (c != NULL) {
		c->data[0]=part;
		memcpy(&(c->data[1]),&(v[part]),4);
		memcpy(&(c->data[5]),&(v[part+1]),4);
		memcpy(&(c->data[9]),&(v[part+2]),4);

		/*printf("to #%d/%d, ",p,part);
		printVector("_du:",du);*/

		if (sendMessageToPort(c, p, c->data, 13, vectorDataMessageHandler, AckCallback) == 0) {
			freeChunk(c);
			//setColor(ORANGE);
			currentBBColor[0]=255;
			currentBBColor[1]=69;
			currentBBColor[2]=0;
			return 0;
		}
	}
	return 1;
}

byte vectorDataMessageHandler(void) {
	if (thisChunk == NULL) return 0;
	byte sender = faceNum(thisChunk);
	float res[6];
	initVector(res);

	byte dec=thisChunk->data[0];
	memcpy(&(res[dec]),&(thisChunk->data[1]),4);
	memcpy(&(res[dec+1]),&(thisChunk->data[5]),4);
	memcpy(&(res[dec+2]),&(thisChunk->data[9]),4);

	if (!started) ForcesPredictionIPPTCode_startup();

	/*printf("%d rec from %d: (%d) (%f,%f,%f,%f,%f,%f)\n",getGUID(),thisNeighborhood.n[sender],sender,res[0],res[1],res[2],res[3],res[4],res[5]);*/
	ForcesPredictionIPPTCode_ProcSendDuFunc(res,sender,dec);
	return 1;
}

void ForcesPredictionIPPTCode_ProcSendDuFunc(float msgData[6], PRef sender, byte dec) {
	/*bID msgFrom = sender->getConnectedBlockBId();
	vector<float> msgData = *(msg->getData());*/
	if(curIteration > maxIterations) {
		return;
	}
/*
	for(int i=0;i<6;i++){
		if(neighbors[i][0]==msgFrom){
			OUTPUT << "Iter=" << curIteration  <<  ", ID="<< module->blockId << " received the message from " << msgFrom<< endl;
			printVector(msgData,6,"msgData from "+to_string(msgFrom)+" to "+ to_string(module->blockId));
			neighbors[i][1]=1;
			uq[i]=msgData;
		}
	}
*/
	if (dec==0) {
		neighbors[sender][1]=1;
		//copyMatrixToVector(uq,sender,msgData);
		copyVectorToMatrix(msgData,uq,sender);
	} else {
		neighbors[sender][1]=2;
		addVectorsToMatrix(msgData,uq,sender);
//		printVector("_uq",uq[sender]);
	}
	/*
	//checking if there are all messages
	bool calculateDu = true;
	for(int i = 0;i<6;i++ ){
		if(neighbors[i][0]!=0 && neighbors[i][1]==0)
			calculateDu = false;
	}
	*/
	//checking if there are all messages
	byte calculateDu = (neighbors[0][0]==0 || neighbors[0][1]==2 || neighbors[0][1]==3);
	size_t i=1;
	while (calculateDu && i<6) {
		calculateDu = (neighbors[i][0]==0 || neighbors[i][1]==2 || neighbors[i][1]==3);
		i++;
	}
	//ForcesPredictionIPPTCode_CheckNeighbors();
	if (calculateDu) {
		//OUTPUT << "Calculating du"<< endl;
		printf("--------------Calculating du-%d---------------\n",getGUID());
		ForcesPredictionIPPTCode_computeDU();
		//visualisation
		if(curIteration % 20==0){
			//setColor(GREEN);
			printf("%d: iter =%d\n",getGUID(),curIteration);
			ForcesPredictionIPPTCode_visualization();
		}
#ifdef BLINK_AQUA
		else if (curIteration % 20==15) {
			currentBBColor[0]=0;
			currentBBColor[1]=255;
			currentBBColor[2]=255;
			//setColor(AQUA);
		}
#endif
		curIteration++;
		//dup=du;
		copyVector(du,dup);

		if (curIteration==maxIterations) {
			ForcesPredictionIPPTCode_visualization();
			if (isSupport) {
	//			setColor(BLUE);
				currentBBColor[0]=0;
				currentBBColor[1]=0;
				currentBBColor[2]=255;
			}
		}
	}
}

void ForcesPredictionIPPTCode_createRevD(bMatrix matrix, bMatrix result) {
	initMatrix(result);
	for(size_t i=0;i<6;i++) {
		result[i][i] = 1.0/matrix[i][i];
	}
}

void ForcesPredictionIPPTCode_createK11(int i,bMatrix mat) {
	/*neightborFromPort={z,-z,-x,x,-y,y};
	{5,0,1,4,2,3}	*/
	float conv[6] = {1,2,5,4,3,0};
	byte iconv = conv[i];
	for(size_t k=0;k<6;k++) {
		for(size_t m=0;m<6;m++) {
			mat[k][m]=K11(iconv,k,m);
		}
	}
	/*printf("i=%d",iconv);
	printMatrix("K11",mat);*/
}

void ForcesPredictionIPPTCode_createK12(int i,bMatrix mat) {
	float conv[6] = {1,2,5,4,3,0};
	byte iconv = conv[i];
	for(size_t k=0;k<6;k++) {
		for(size_t m=0;m<6;m++){
			mat[k][m]=K12(iconv,k,m);
		}
	}
}

void ForcesPredictionIPPTCode_createRot(int i,bMatrix mat) {
	float conv[6] = {1,2,5,4,3,0};
	byte iconv = conv[i];

	for(size_t k=0;k<6;k++)
		for(size_t m=0;m<6;m++){
			mat[k][m]=Rot(iconv,k,m);
		}
}

void ForcesPredictionIPPTCode_createD(bMatrix mat, bMatrix result) {
	initMatrix(result);
	for(size_t i=0; i<6;i++){
		result[i][i] = mat[i][i]; // operation
	}
}

void ForcesPredictionIPPTCode_createR(bMatrix mat, bMatrix result) {
	bMatrix tmp;
	ForcesPredictionIPPTCode_createD(mat,tmp);

	for(size_t i=0; i<6;i++) {
		for(size_t j=0; j<6;j++) {
			result[i][j] = mat[i][j] - tmp[i][j];
		}
	}
}

void ForcesPredictionIPPTCode_createTfr(float Txx, float Txy, float Txz,float Tyx, float Tyy, float Tyz, bMatrix res) {
    identityMatrix(res);
    res[0][0]=Txx;    res[0][1]=Txy;    res[0][2]=Txz;
    res[1][0]=Tyx;    res[1][1]=Tyy;    res[1][2]=Tyz;
}

void ForcesPredictionIPPTCode_createTmx(float Txz,bMatrix res) {
    identityMatrix(res);
    res[3][3]=0;
    res[3][2]=Txz;
}

void ForcesPredictionIPPTCode_createTmy(float Txz,bMatrix res) {
    identityMatrix(res);
    res[4][4]=0;
    res[4][2]=Txz;
}

void ForcesPredictionIPPTCode_createTmz(float Txz,bMatrix res) {
    identityMatrix(res);
    res[5][5]=0;
    res[5][2]=Txz;
}

void ForcesPredictionIPPTCode_visualization() {
	//calculate only of not support
	if(isFixed)	return;

	float fxMaxV = 20.5*grav*mass; // max force in N (for up and down direction)
	float fxMaxL = 25.5*grav*mass; // max force in N (for lateral directions)

	bMatrix tmpK11;
	initMatrix(tmpK11);
	bMatrix tmpK12;
	initMatrix(tmpK12);
	float res1[6];
	float res2[6];
	bMatrix mrot;
//	bMatrix R = decltype(R)(vectorSize, vector<float>(vectorSize));

	for(size_t i=0;i<6;i++){
		if (neighbors[i][0]!=0 && !isFixed) {
			ForcesPredictionIPPTCode_createK11(i,tmpK11);
			ForcesPredictionIPPTCode_createK12(i,tmpK12);
			// ---vizTable[i]=createRot(i)*(tmpK11*dup+tmpK12*uq[i]); ----
			multMatrixVector(tmpK11,dup,res1);
			multMatrixVector(tmpK12,uq[i],res2);
			addVectors(res1,res2,res1);
			ForcesPredictionIPPTCode_createRot(i,mrot);
			multMatrixVector(mrot,res1,vizTable[i]);

			int di=1-2*(i%2);
   			ForcesPredictionIPPTCode_createK11(i+di,tmpK11);
    		ForcesPredictionIPPTCode_createK12(i+di,tmpK12);
			// --- vizTable[i][4]=(vizTable[i][4]+(createRot(i+di)*(tmpK11*uq[i]+tmpK12*dup))[4])/2;
			multMatrixVector(tmpK11,uq[i],res1);
			multMatrixVector(tmpK12,dup,res2);
			addVectors(res1,res2,res1);

			ForcesPredictionIPPTCode_createRot(i+di,mrot);
			multMatrixVector(mrot,res1,res2);
			vizTable[i][4]=(vizTable[i][4]+res2[4])/2.0;

   			// --- vizTable[i][5]=(vizTable[i][5]+(createRot(i+di)*(tmpK11*uq[i]+tmpK12*dup))[5])/2;
			vizTable[i][5]=(vizTable[i][5]+res2[5])/2.0;
		}

	}

	//printMatrix(vizTable,6,6,"VizTable "+to_string(module->blockId));
	//printMatrix(tmpK11,6,6,"tmpK11 "+to_string(module->blockId));
	//printMatrix(tmpK12,6,6,"tmpK12 "+to_string(module->blockId));

	//searching max
	float maxS = 0.0;
	if(isSupport) { // enforce the unilateral contact conditions with the support, located below the module
		// vector<double> Fd=contactStiffnessMatrix(dup)*dup;
		float Fd[6];
		bMatrix mtmp;
		ForcesPredictionIPPTCode_contactStiffnessMatrix(dup,mtmp);
		multMatrixVector(mtmp,dup,Fd);
		float fx=Fd[0], fy=Fd[1], fz=Fd[2], mx=Fd[3], my=Fd[4], mz=Fd[5]; // in global coordinates
		vizTable[1][0]=fx;  vizTable[1][1]=fy;  vizTable[1][2]=fz;  vizTable[1][3]=mx;  vizTable[1][4]=my;  vizTable[1][5]=mz;
		if(fx<0.0) {
			maxS = min(1.,max(fabs(mx)/fabs(fz*_L/2),fabs(my)/fabs(fz*_L/2)));
		}
	}
	//printMatrix(vizTable,6,6,"VizTable "+to_string(module->blockId));

	// chacking the maximum load factor
	for(size_t i = 0; i<6; i++) {
		float fxMeff = (i==UP || i==DOWN)?fxMaxV:fxMaxL;
		//float fxMeff=((i<2)?fxMaxV:fxMaxL); // effective value of fxMax depending on the connection's type (vertical or lateral)
		if (neighbors[i][0]!=0 && !isFixed) {
			//set abs values of my and mz
			vizTable[i][4] = fabs(vizTable[i][4]);
			vizTable[i][5] = fabs(vizTable[i][5]);

			if(vizTable[i][0]>fxMeff) vizTable[i][0] = fxMeff;

			float mxMeff=(fxMeff-vizTable[i][0]) * _a/2; //effective value of mxMax depending on the connection's type (vertical or lateral)
			if(vizTable[i][0]<0) // this must be executed AFTER the definition of mxMeff
			if(vizTable[i][4]>mxMeff) vizTable[i][4] = mxMeff;
			if(vizTable[i][5]>mxMeff) vizTable[i][5] = mxMeff;

	        maxS=max(vizTable[i][0]/fxMeff, maxS);
	        maxS=max(vizTable[i][4]/mxMeff, maxS);
	        maxS=max(vizTable[i][5]/mxMeff, maxS);
		}
	}
	//OUTPUT << "Module " << module->blockId << " level of danger = "<< maxS << endl;

	//set color for module
	//cout << min(2*color,1.) << " " << min(2*(1-color),1.) << endl;
	//printf("%d:%d,%d\n",getGUID(),(int)(min(2*maxS,1.)*255),(int)(min(2*(1-maxS),1.)*255));
	//setLED(min(2*maxS,1.)*255,min(2*(1-maxS),1.)*255,0,255);
	currentBBColor[0]=min(2*maxS,1.)*255;
	currentBBColor[1]=255-currentBBColor[0];
	currentBBColor[2]=0;
	if (curIteration==maxIterations && maxS>=1.-Eps) {
		blinkMode=1;
	}
}

void initMatrix(bMatrix mat) {
	for (size_t i=0; i<6; i++) {
		for (size_t j=0; j<6; j++) {
			mat[i][j] = 0.0;
		}
	}
}

void identityMatrix(bMatrix mat) {
	for (size_t i=0; i<6; i++) {
		for (size_t j=0; j<6; j++) {
			mat[i][j] = (i==j)?1.0:0.0;
		}
	}
}


void initVector(float vec[6]) {
	for (size_t i=0; i<6; i++) {
		vec[i] = 0.0;
	}
}

void copyVector(float src[6],float res[6]) {
	for (size_t i=0; i<6; i++) {
		res[i] = src[i];
	}
}

void multVectConst(float vec[6],float scal,float res[6]) {
	for (size_t i=0; i<6; i++) {
		res[i] = vec[i]*scal;
	}
}

void addVectors(float *vec1,float *vec2,float *res) {
	for (size_t i=0; i<6; i++) {
		res[i] = vec1[i]+vec2[i];
	}
}

void subVectors(float *vec1,float *vec2,float *res) {
	for (size_t i=0; i<6; i++) {
		res[i] = vec1[i]-vec2[i];
	}
}

void multMatrixConst(bMatrix mat1,float cst,bMatrix res) {
	for (size_t i=0;i<6;i++) {
		for (size_t j=0;j<6;j++) {
			res[i][j]=mat1[i][j] * cst;
		}
	}
}

void multMatrixVector(bMatrix mat,float vec[6],float res[6]) {
	float tmp[6];
	initVector(tmp);
	for (size_t i=0;i<6;i++) {
		for (size_t j=0;j<6;j++) {
			tmp[i]+=(mat[i][j]*vec[j]);
		}
	}
	copyVector(tmp,res);
}

void printVector(char *titre,float vec[6]) {
	printf("%d:%s(%f,%f,%f,%f,%f,%f)\n",getGUID(),titre,vec[0],vec[1],vec[2],vec[3],vec[4],vec[5]);
}

void printMatrix(char *titre,bMatrix mat) {
	printf("---%d:%s---\n",getGUID(),titre);
	for (size_t i=0;i<6;i++) {
		printf("(");
		for (size_t j=0;j<6;j++) {
			printf("%f,",mat[i][j]);
		}
		printf(")\n");
	}
}

void addMatrices(bMatrix mat1,bMatrix mat2,bMatrix res) {
	for (size_t i=0;i<6;i++) {
		for (size_t j=0;j<6;j++) {
			res[i][j]=mat1[i][j]+mat2[i][j];
		}
	}
}

void addVectorsToMatrix(float *vec,bMatrix mat,byte col) {
	for (size_t i=0;i<6;i++) {
		mat[col][i]+=vec[i];
	}
}

void copyMatrixToVector(bMatrix mat,byte col,float res[6]) {
	for (size_t i=0;i<6;i++) {
		res[i] = mat[col][i];
	}
}

void copyVectorToMatrix(float res[6],bMatrix mat,byte col) {
	for (size_t i=0;i<6;i++) {
		mat[col][i]=res[i];
	}
}

void copyMatrix(bMatrix mat,bMatrix res) {
	for(int i=0; i<6;i++) {
		for(int j=0;j<6;j++) {
			res[i][j] = mat[i][j];
		}
	}
}

void multMatrices(bMatrix mat1,bMatrix mat2,bMatrix res){
	bMatrix tmp;
	initMatrix(tmp);
	for(int i=0; i<6;i++){
		for(int j=0;j<6;j++)	{
			for(int k=0;k<6;k++){
				tmp[i][j] += mat1[i][k] * mat2[k][j]; // operation
			}
		}
	}
	copyMatrix(tmp,res);
}


void ForcesPredictionIPPTCode_CheckNeighbors() {
	printf("neighbors for id= %d\n",getGUID());
	for(int i=0;i<6;i++) {
		printf("%d,",neighbors[i][0]);
	}
	printf("\n");
	for(int i=0;i<6;i++){
		printf("%d,",neighbors[i][1]);
	}
	printf("\n");
}


void AckCallback(void) {
  if (chunkResponseType(thisChunk) != MSG_RESP_ACK) { // good reception
	printf("%d NOK\n",getGUID());
	//setColor(PINK);
	currentBBColor[0]=255;
	currentBBColor[1]=192;
	currentBBColor[2]=203;

	//ForcesPredictionIPPTCode_sendMessageToAllNeighbors(du);
	byte p = faceNum(thisChunk);
	sendVectorPartToPort(p,du,0);
	sendVectorPartToPort(p,du,3);
  };
  freeChunk(thisChunk);
}

void AckCoordCallback(void) {
  if (chunkResponseType(thisChunk) != MSG_RESP_ACK) { // good reception
	printf("%d NOK\n",getGUID());
	//setColor(PINK);
	currentBBColor[0]=255;
	currentBBColor[1]=192;
	currentBBColor[2]=203;
  };
  freeChunk(thisChunk);
}
