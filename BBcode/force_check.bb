#include <math.h>
#include "block.bbh"
#include "clock.bbh"

threadtype typedef float bMatrix[6][6];

threadvar int maxIterations = 2000; // max number of iterations
threaddef #define _E	100.0
threaddef #define _L	40.0
threaddef #define _A	1600.0
threaddef #define _I	213333.333333333
threaddef #define _Iz	213333.333333333
threaddef #define _Iy	213333.333333333
threaddef #define _nu	0.3
threaddef #define _J	360000.0

threaddef #define mass	61.03
threaddef #define grav	9.81
threaddef #define beta	2.0/3.0

threaddef #define Omega	2.0/3.0
threaddef #define Mu	0.1
threaddef #define Eps	1.0E-8
threaddef #define Gamma	1.0E-6
threaddef #define supportZ	127

threaddef #define K11(i,j,k)((const float[6][6][6]) {{{12.*_E*_Iy/(_L*_L*_L), 0, 0, 0, 6.*_E*_Iy/(_L*_L), 0},{0, 12.*_E*_Iz/(_L*_L*_L), 0, -6*_E*_Iz/(_L*_L), 0, 0},{0, 0, _A*_E/_L, 0, 0, 0},{0, -6*_E*_Iz/(_L*_L), 0 , 4*_E*_Iz/_L, 0, 0},{6*_E*_Iy/(_L*_L), 0, 0, 0, 4*_E*_Iy/_L, 0},{0, 0, 0, 0, 0, _E*_J/(2*_L*(1*_nu))}},{{12.*_E*_Iy/(_L*_L*_L), 0, 0, 0, -6.*_E*_Iy/(_L*_L), 0},{0, 12.*_E*_Iz/(_L*_L*_L), 0, 6*_E*_Iz/(_L*_L), 0, 0},{0, 0, _A*_E/_L, 0, 0, 0},{0, 6*_E*_Iz/(_L*_L), 0 , 4*_E*_Iz/_L, 0, 0},{-6*_E*_Iy/(_L*_L), 0, 0, 0, 4*_E*_Iy/_L, 0},{0, 0, 0, 0, 0, _E*_J/(2*_L*(1*_nu))}},{{_A*_E/_L, 0, 0, 0, 0, 0},{0, 12*_E*_Iz/(_L*_L*_L),  0, 0, 0, -6*_E*_Iz/(_L*_L)},{0, 0, 12*_E*_Iy/(_L*_L*_L), 0, 6*_E*_Iy/(_L*_L), 0},{0, 0, 0, _E*_J/(2*_L*(1+_nu)), 0, 0},{0, 0, 6*_E*_Iy/(_L*_L), 0, 4*_E*_Iy/_L, 0},{0, -6*_E*_Iz/(_L*_L), 0, 0, 0, 4*_E*_Iz/_L}},{{_A*_E/_L, 0, 0, 0, 0, 0},{0, 12*_E*_Iz/(_L*_L*_L),  0, 0, 0, 6*_E*_Iz/(_L*_L)},{0, 0, 12*_E*_Iy/(_L*_L*_L), 0, -6*_E*_Iy/(_L*_L), 0},{0, 0, 0, _E*_J/(2*_L*(1+_nu)), 0, 0},{0, 0, -6*_E*_Iy/(_L*_L), 0, 4*_E*_Iy/_L, 0},{0, 6*_E*_Iz/(_L*_L), 0, 0, 0, 4*_E*_Iz/_L}},{{12.*_E*_Iz/(_L*_L*_L), 0, 0, 0, 0, 6*_E*_Iz/(_L*_L)},{0,_A*_E/_L, 0, 0, 0, 0},{0, 0, 12*_E*_Iy/(_L*_L*_L), -6*_E*_Iy/(_L*_L), 0, 0},{0, 0, -6*_E*_Iy/(_L*_L), 4*_E*_Iy/_L, 0, 0},{0, 0, 0, 0, _E*_J/(2*_L*(1+_nu)), 0},{6*_E*_Iz/(_L*_L), 0, 0, 0, 0, 4*_E*_Iz/_L}},{{12.*_E*_Iz/(_L*_L*_L), 0, 0, 0, 0, -6*_E*_Iz/(_L*_L)},{0,_A*_E/_L, 0, 0, 0, 0},{0, 0, 12*_E*_Iy/(_L*_L*_L), 6*_E*_Iy/(_L*_L), 0, 0},{0, 0, 6*_E*_Iy/(_L*_L), 4*_E*_Iy/_L, 0, 0},{0, 0, 0, 0, _E*_J/(2*_L*(1+_nu)), 0},{-6*_E*_Iz/(_L*_L), 0, 0, 0, 0, 4*_E*_Iz/_L}}}[i][j][k])

threaddef #define K12(i,j,k)((const float[6][6][6]) {{{-12.*_E*_Iy/(_L*_L*_L), 0, 0, 0, 6*_E*_Iy/(_L*_L),0},{0,-12.*_E*_Iz/(_L*_L*_L), 0, -6*_E*_Iz/(_L*_L), 0, 0},{0, 0, -_A*_E/_L, 0, 0, 0},{0, 6*_E*_Iz/(_L*_L), 0, 2*_E*_Iz/_L, 0, 0},{-6*_E*_Iy/(_L*_L), 0 ,0, 0, 2*_E*_Iy/_L, 0},{0, 0, 0, 0, 0, -_E*_J/(2*_L*(1+_nu))}},{{-12.*_E*_Iy/(_L*_L*_L), 0, 0, 0, -6*_E*_Iy/(_L*_L),0},{0,-12.*_E*_Iz/(_L*_L*_L), 0, 6*_E*_Iz/(_L*_L), 0, 0},{0, 0, -_A*_E/_L, 0, 0, 0},{0, -6*_E*_Iz/(_L*_L), 0, 2*_E*_Iz/_L, 0, 0},{6*_E*_Iy/(_L*_L), 0 ,0, 0, 2*_E*_Iy/_L, 0},{0, 0, 0, 0, 0, -_E*_J/(2*_L*(1+_nu))}},{{-_A*_E/_L, 0, 0, 0, 0, 0},{0, -12*_E*_Iz/(_L*_L*_L),  0, 0, 0, -6*_E*_Iz/(_L*_L)},{0, 0, -12*_E*_Iy/(_L*_L*_L), 0, 6*_E*_Iy/(_L*_L), 0},{0, 0, 0, -_E*_J/(2*_L*(1+_nu)), 0, 0},{0, 0, -6*_E*_Iy/(_L*_L), 0, 2*_E*_Iy/_L, 0},{0, 6*_E*_Iz/(_L*_L), 0, 0, 0, 2*_E*_Iz/_L}},{{-_A*_E/_L, 0, 0, 0, 0, 0},{0, -12*_E*_Iz/(_L*_L*_L),  0, 0, 0, 6*_E*_Iz/(_L*_L)},{0, 0, -12*_E*_Iy/(_L*_L*_L), 0, -6*_E*_Iy/(_L*_L), 0},{0, 0, 0, -_E*_J/(2*_L*(1+_nu)), 0, 0},{0, 0, 6*_E*_Iy/(_L*_L), 0, 2*_E*_Iy/_L, 0},{0, -6*_E*_Iz/(_L*_L), 0, 0, 0, 2*_E*_Iz/_L}},{{-12.*_E*_Iz/(_L*_L*_L), 0, 0, 0, 0, 6*_E*_Iz/(_L*_L)},{0, -_A*_E/_L, 0, 0, 0, 0},{0, 0, -12*_E*_Iy/(_L*_L*_L), -6*_E*_Iy/(_L*_L), 0, 0},{0, 0, 6*_E*_Iy/(_L*_L), 2*_E*_Iy/_L, 0, 0},{0, 0, 0, 0, -_E*_J/(2*_L*(1+_nu)), 0},{-6*_E*_Iz/(_L*_L), 0, 0, 0, 0, 2*_E*_Iz/_L}},{{-12.*_E*_Iz/(_L*_L*_L), 0, 0, 0, 0, -6*_E*_Iz/(_L*_L)},{0, -_A*_E/_L, 0, 0, 0, 0},{0, 0, -12*_E*_Iy/(_L*_L*_L), 6*_E*_Iy/(_L*_L), 0, 0},{0, 0, -6*_E*_Iy/(_L*_L), 2*_E*_Iy/_L, 0, 0},{0, 0, 0, 0, -_E*_J/(2*_L*(1+_nu)), 0},{6*_E*_Iz/(_L*_L), 0, 0, 0, 0, 2*_E*_Iz/_L}}}[i][j][k])

threaddef #define Rot(i,j,k)((const float[6][6][6])	{{{0,0,1,0,0,0},{0,1,0,0,0,0},{-1,0,0,0,0,0},{0,0,0,0,0,1},{0,0,0,0,1,0},{0,0,0,-1,0,0}},{{0,0,-1,0,0,0},{0,1,0,0,0,0},{1,0,0,0,0,0},{0,0,0,0,0,-1},{0,0,0,0,1,0},{0,0,0,1,0,0}},{{-1,0,0,0,0,0},{0,-1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,-1,0,0},{0,0,0,0,-1,0},{0,0,0,0,0,1}},{{1,0,0,0,0,0},{0,1,0,0,0,0},{0,0,1,0,0,0},{0,0,0,1,0,0},{0,0,0,0,1,0},{0,0,0,0,0,1}},{{0,-1,0,0,0,0},{1,0,0,0,0,0},{0,0,1,0,0,0},{0,0,0,0,-1,0},{0,0,0,1,0,0},{0,0,0,0,0,1}},{{0,1,0,0,0,0},{-1,0,0,0,0,0},{0,0,1,0,0,0},{0,0,0,0,1,0},{0,0,0,-1,0,0},{0,0,0,0,0,1}}}[i][j][k])

threadvar byte neighbors[6][2];// Neighbors 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1 ///// second row  if tehre is a message with du

threadvar byte support = 0;
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
threadvar byte currentColor[3];

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
void ForcesPredictionIPPTCode_SetNeighbors(void);
void ForcesPredictionIPPTCode_calculateU(void);
byte ForcesPredictionIPPTCode_isFixed(void);
byte ForcesPredictionIPPTCode_isSupport(void);
void ForcesPredictionIPPTCode_visualization(void);

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

byte sendVectorPartToPort(PRef p,float v[6],byte part);
byte vectorDataMessageHandler(void);
//void parseUserElements(TiXmlDocument* config);
//void parseUserBlockElements(TiXmlElement* config);

void initMatrix(bMatrix mat);
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
void printVector(char *str,float vec[6]);
void printMatrix(char *titre,bMatrix mat);

float max(float a,float b) { return (a>b)?a:b; };
float min(float a,float b) { return (a<b)?a:b; };

threaddef #define MYCHUNKS 24
threadextern Chunk* thisChunk;
threadvar Chunk myChunks[MYCHUNKS];

void myMain(void) {
	started=0;
	setLED(0,0,0,0);
	position[0] = 0;
	position[1] = 0;
	position[2] = 0;
	currentColor[0]=0;
	currentColor[1]=0;
	currentColor[2]=0;
	// Problems with BBsim, must reinit !
	tree_parent=NO_PARENT;
	nbreWaitedAnswers=0;
	blinkMode=0;
	xplusBorder=SOUTH;
	yplusBorder=EAST;
	zplusBorder=UP;
	support=0;
	curIteration = 0;
	maxIterations = 2000;

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
	// vector<double> orient={0,0,-1,0,0,0};
	initVector(orient);
	orient[2]=-1.0f;
/*
	mass = 61.03; //mass
	grav = 9.81; //gravity
	beta = 2.0/3.0; //beta

	Omega = 2.0/3.0; // weight of Jacobi method
	Mu = 0.1; //friction coefficient
	Eps = 1.0E-8; // tolerance pow(10,-8);
	Gamma = 1.0E-6; //stiffness reduction multiplier (for unilateral contact)
	supportZ = 127; //Z coordinate of the bottom modules (contacting with the support)
*/
	//cheking neighbors and adding them to a list
	ForcesPredictionIPPTCode_SetNeighbors();

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
		/*setColor((position[2]-1) % 9);
		delayMS(100);

		if (blinkMode) {
		    delayMS(900);
            setColor(WHITE);
		}*/
		setLED(currentColor[0],currentColor[1],currentColor[2],255);
		delayMS(100);
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
			blinkMode=1;
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

/* FORCES CALCULATION */
void ForcesPredictionIPPTCode_calculateU() {
	//temporary Matrixes
	bMatrix tmpK11;
	initMatrix(tmpK11);
	float tmpF12[6];
	initVector(tmpF12);

	//char str[12];

	if(!ForcesPredictionIPPTCode_isFixed()) {
		//checking neighbors and creating K11 and K12 matrixes
		bMatrix mtmp;
		float vtmp[6];
		for(int i=0;i<6;i++){
			if(neighbors[i][0]!=0){
				//OUTPUT << module->blockId << "has neighbor" << neighbors[i] << endl;
				// ---- tmpK11=tmpK11+createK11(i); ----
				ForcesPredictionIPPTCode_createK11(i,mtmp);
				addMatrices(tmpK11,mtmp,tmpK11);
				// ---- tmpF12 = tmpF12+(createK12(i)*uq[i]); ----
				ForcesPredictionIPPTCode_createK12(i,mtmp);
				multMatrixVector(mtmp,uq[i],vtmp);
				addVectors(tmpF12,vtmp,tmpF12);
/*				sprintf(str,"_uq[%d]=",i);
				printVector(str,uq[i]);*/
			}
		}
//		printVector("tmpF12",tmpF12);
		//creating R and D
		bMatrix R; //R vector
		bMatrix D; //D vector
		bMatrix revD; //revD vector
		ForcesPredictionIPPTCode_createD(tmpK11,D);
		ForcesPredictionIPPTCode_createR(tmpK11,R);
		ForcesPredictionIPPTCode_createRevD(D,revD);
		float tmp[6]; //tmp u
		initVector(tmp);
		float tmp1[6];
		initVector(tmp1);
		bMatrix tmpBD;
		//Beta * revD
		// ---- tmpBD = revD*beta; ----
		multMatrixConst(revD,beta,tmpBD);
		//Fp - fp
		// ---- tmp = Fp+(fp*-1.); ----
		subVectors(Fp,fp,tmp);
		//add Ru part
		// ---- tmp1 = R*dup; ----
		multMatrixVector(R,dup,tmp1);
		// ---- tmp1 = tmp1*-1.;
		// tmp = tmp+tmp1; ----
		subVectors(tmp,tmp1,tmp);
		// add K12 part
		// ---- tmp1 = tmpF12*-1.;
		// tmp = tmp+tmp1; ----
		subVectors(tmp,tmpF12,tmp);
		//bd * ()
		// ---- tmp = tmpBD*tmp; ----
		multMatrixVector(tmpBD,tmp,tmp);
		//bd*()-(1-B)u-1
		// ---- tmp=tmp+(dup*(1-beta));
		// du=tmp; ----
		multVectConst(dup,1.0-beta,vtmp);
		addVectors(tmp,vtmp,du);
//		printVector(du,6,"vector du module id= " + to_string(module->blockId) + " interaction "+ to_string(curIteration));
		printVector("vector _du ",du);
	}	else { //end isFixed
		initVector(du);
	}

	//sending message to neighbors with du
	//OUTPUT << "size=" << du.size() << endl;
	// ---- sendMessageToAllNeighbors("DU_MSG",new MessageOf<vector<float>>(DU_MSG,du),messageDelay,messageDelayError,0);

	//clearing info about du from neighbors
	ForcesPredictionIPPTCode_clearNeighborsMessage();
	ForcesPredictionIPPTCode_sendMessageToAllNeighbors(du);
}

void ForcesPredictionIPPTCode_SetNeighbors() {
	printf("setNeighbors %d\n",getGUID());
	//set 0 for all empty neighbors
	size_t n=0;
	do {
		n=0;
		for(size_t i=0;i<6;i++) {
			neighbors[i][0] = (thisNeighborhood.n[i]!=VACANT);
			neighbors[i][1] = 0;
			if (neighbors[i][0]) n++;
		}
	} while (n!=getNeighborCount());
	//ForcesPredictionIPPTCode_CheckNeighbors();
/*
	neighbors[2][0] = thisNeighborhood.n[5-xplusBorder];
	neighbors[3][0] = thisNeighborhood.n[xplusBorder];
	neighbors[4][0] = thisNeighborhood.n[5-yplusBorder];
	neighbors[5][0] = thisNeighborhood.n[yplusBorder];
	neighbors[0][0] = thisNeighborhood.n[zplusBorder];
	neighbors[1][0] = thisNeighborhood.n[5-zplusBorder];*/
}

void ForcesPredictionIPPTCode_startup() {
	started=1;
	//check is module fixed
	if(ForcesPredictionIPPTCode_isFixed()) {
	//setColor(RED);
		currentColor[0]=255;
		currentColor[1]=0;
		currentColor[2]=0;
    }
	//check is module support
	support = ForcesPredictionIPPTCode_isSupport();

	//createK11(K11);
	//createK12(K12);

	//first step - calculate DU and sends to neighbor only in first
	if(curIteration == 0){
		ForcesPredictionIPPTCode_calculateU();
		curIteration++;
	}
}

byte ForcesPredictionIPPTCode_isFixed() {
	return (position[2]==127);
}

byte ForcesPredictionIPPTCode_isSupport() {
	if(position[2]==supportZ){
		// setColor(WHITE);
		currentColor[0]=255;
		currentColor[1]=255;
		currentColor[2]=255;
		return 1;
	}
	return 0;
}

void ForcesPredictionIPPTCode_clearNeighborsMessage() {
	printf("%d rec clear\n",getGUID());
	for(size_t i=0;i<6;i++) {
		neighbors[i][1]=0;
	}
}

void ForcesPredictionIPPTCode_sendMessageToAllNeighbors(float v[6]) {
	printf("%d: Sends (%f,%f,%f,%f,%f,%f) to ",getGUID(),v[0],v[1],v[2],v[3],v[4],v[5]);
	for(size_t i=0;i<6;i++) {
		if (neighbors[i][0]) {
			printf("%d(%d),",thisNeighborhood.n[i],i);
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
			currentColor[0]=255;
			currentColor[1]=69;
			currentColor[2]=0;
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
	byte calculateDu = (neighbors[0][0]==0 || neighbors[0][1]==2);
	size_t i=1;
	while (calculateDu && i<6) {
		calculateDu = (neighbors[i][0]==0 || neighbors[i][1]==2);
		i++;
	}
	//ForcesPredictionIPPTCode_CheckNeighbors();
	if (calculateDu) {
		//OUTPUT << "Calculating du"<< endl;
		printf("--------------Calculating du-%d---------------\n",getGUID());
		ForcesPredictionIPPTCode_calculateU();
		//visualisation
		if(curIteration % 20==0){
			//setColor(GREEN);
			printf("%d: iter =%d\n",getGUID(),curIteration);
			ForcesPredictionIPPTCode_visualization();
		} else if (curIteration % 20==15) {
			currentColor[0]=0;
			currentColor[1]=255;
			currentColor[2]=255;
			setColor(AQUA);
		}
		curIteration++;
		//dup=du;
		copyVector(du,dup);

		if (curIteration==maxIterations) {
			if (support) {
	//			setColor(BLUE);
				currentColor[0]=0;
				currentColor[1]=0;
				currentColor[2]=255;
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

void ForcesPredictionIPPTCode_visualization() {
	//calculate only of not support
	if(support)	return;

	float fxMax = 25.5*grav*mass; // max force
	float mxMax = fxMax * _L/2; // max moment

	bMatrix tmpK11;
	initMatrix(tmpK11);
	bMatrix tmpK12;
	initMatrix(tmpK12);
	float res1[6];
	float res2[6];
	bMatrix mrot;
//	bMatrix R = decltype(R)(vectorSize, vector<double>(vectorSize));

	for(size_t i=0;i<6;i++){
		if (neighbors[i][0]!=0){
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
			vizTable[i][4]=(vizTable[i][4]+res2[4])/2;

   			// --- vizTable[i][5]=(vizTable[i][5]+(createRot(i+di)*(tmpK11*uq[i]+tmpK12*dup))[5])/2;
			vizTable[i][5]=(vizTable[i][5]+res2[5])/2;
		}

	}

	//printMatrix(vizTable,6,6,"VizTable "+to_string(module->blockId));
	//printMatrix(tmpK11,6,6,"tmpK11 "+to_string(module->blockId));
	//printMatrix(tmpK12,6,6,"tmpK12 "+to_string(module->blockId));

	//searching max
	float maxS = 0;
	for(size_t i = 0; i<6; i++) {
		if(vizTable[i][0]<0) vizTable[i][0] = 0;
		//set abs values of my and mz
		vizTable[i][4] = fabs(vizTable[i][4]);
		vizTable[i][5] = fabs(vizTable[i][5]);

		if(vizTable[i][0]>fxMax) vizTable[i][0] = fxMax;
		if(vizTable[i][4]>mxMax) vizTable[i][4] = mxMax;
		if(vizTable[i][5]>mxMax) vizTable[i][5] = mxMax;

        maxS=max(vizTable[i][0]/fxMax, maxS);
        maxS=max(vizTable[i][4]/mxMax, maxS);
        maxS=max(vizTable[i][5]/mxMax, maxS);
	}
	//OUTPUT << "Module " << module->blockId << " level of danger = "<< maxS << endl;

	//set color for module
	//cout << min(2*color,1.) << " " << min(2*(1-color),1.) << endl;
	//printf("%d:%d,%d\n",getGUID(),(int)(min(2*maxS,1.)*255),(int)(min(2*(1-maxS),1.)*255));
	//setLED(min(2*maxS,1.)*255,min(2*(1-maxS),1.)*255,0,255);
	currentColor[0]=min(2*maxS,1.)*255;
	currentColor[1]=255-currentColor[0];
	currentColor[2]=0;
}


void initMatrix(bMatrix mat) {
	for (size_t i=0; i<6; i++) {
		for (size_t j=0; j<6; j++) {
			mat[i][j] = 0.0;
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
  if (!chunkResponseType(thisChunk) == MSG_RESP_ACK) { // good reception
	printf("%d NOK\n",getGUID());
	//setColor(PINK);
	currentColor[0]=255;
	currentColor[1]=192;
	currentColor[2]=203;

	//ForcesPredictionIPPTCode_sendMessageToAllNeighbors(du);
	byte p = faceNum(thisChunk);
	sendVectorPartToPort(p,du,0);
	sendVectorPartToPort(p,du,3);
  };
  freeChunk(thisChunk);
}

void AckCoordCallback(void) {
  if (!chunkResponseType(thisChunk) == MSG_RESP_ACK) { // good reception
	printf("%d NOK\n",getGUID());
	//setColor(PINK);
	currentColor[0]=255;
	currentColor[1]=192;
	currentColor[2]=203;
  };
  freeChunk(thisChunk);
}
