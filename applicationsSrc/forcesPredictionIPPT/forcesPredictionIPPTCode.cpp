#include "forcesPredictionIPPTCode.h"

const int messageDelay=50;
const int messageDelayError=5;
const int messageDelayCons=1;


int maxIterations = 2; // max number of iterations
double globalMass = 61/1000; //mass from XML
double globalE = 100; // E from XML // Young modulus MPa
double globalL=40; //length from XML // arm length mm
double globala = 40; //width of the square-cross-section arm  mm //
double globalA=globala*globala; //cross sectional area from XML mm^2
double globalI=pow(globala,4)/12.; // second moment of area from XML mm^4
double globalIz = globalI; //second moment of area
double globalIy = globalI; //second moment of area
double globalNu = 0.3; //Poisson ratio
double globalJ=2.25*pow((globala/2),4); //torsion constant

double globalGrav=9.81; //gravity from XML
double globalBeta=2/3.; //beta from XML

double globalOmega = 2./3; // weight of Jacobi method
double globalMu = 0.1; //friction coefficient
double globalEps = pow(10,-8); // //tolerance
double globalGamma = pow(10,-6); //stiffness reduction multiplier (for unilateral contact)
double globalSupportZ = 0; //Z coordinate of the bottom modules (contacting with the support)




/* parse XML files extra data */
/* be carefull, is run only one time by the first module! */
void ForcesPredictionIPPTCode::parseUserElements(TiXmlDocument* config) {
	TiXmlNode *node = config->FirstChild("parameters");


	cerr << "blockId=" << module->blockId << endl;
	TiXmlElement* element = node->ToElement();
	const char *attr= element->Attribute("globalSupportZ");
	//mass of module
	if (attr) {
		string str=attr;
		globalSupportZ = atof(str.c_str());
		cerr << "globalSupportZ= " << globalSupportZ << endl;
	} else {
			OUTPUT << "WARNING No globalSupportZ in XML file" << endl;
	}


	attr= element->Attribute("globalGamma");
	//mass of module
	if (attr) {
		string str=attr;
		globalGamma = atof(str.c_str());
		cerr << "globalGamma= " << globalGamma << endl;
	} else {
			OUTPUT << "WARNING No globalGamma in XML file" << endl;
	}

	attr= element->Attribute("globalEps");
	//mass of module
	if (attr) {
		string str=attr;
		globalEps = atof(str.c_str());
		cerr << "globalEps= " << globalEps << endl;
	} else {
			OUTPUT << "WARNING No globalEps in XML file" << endl;
	}

	attr= element->Attribute("globalMu");
	//mass of module
	if (attr) {
		string str=attr;
		globalMu = atof(str.c_str());
		cerr << "globalMu= " << globalMu << endl;
	} else {
			OUTPUT << "WARNING No globalMu in XML file" << endl;
	}


	attr= element->Attribute("globalOmega");
	if (attr) {
		string str=attr;
		globalOmega = atof(str.c_str());
		cerr << "globalOmega= " << globalOmega << endl;
	} else {
			OUTPUT << "WARNING No globalOmega in XML file" << endl;
	}

	attr= element->Attribute("globalMass");
	if (attr) {
		string str=attr;
		globalMass = atof(str.c_str());
		cerr << "Mass= " << globalMass << endl;
	} else {
			OUTPUT << "WARNING No mass in XML file" << endl;
	}

	attr= element->Attribute("nofIterations");
	if (attr) {
		string str=attr;
		maxIterations = atoi(str.c_str());
		cerr << "maxNofIterations= " << maxIterations << endl;
	} else {
			OUTPUT << "WARNING No maxNofIterations in XML file" << endl;
	}

	attr= element->Attribute("a");
	if (attr) {
		string str=attr;
		globala = atof(str.c_str());
		cerr << "a= " << globala << endl;
	} else {
			OUTPUT << "WARNING No a in XML file" << endl;
	}


	attr= element->Attribute("E");
		if (attr) {
			string str=attr;
			globalE = atof(str.c_str());
			cerr << "E= " << globalE << endl;
	} else {
			OUTPUT << "WARNING No E in XML file" << endl;
	}

	attr= element->Attribute("A");
		if (attr) {
			string str=attr;
			globalA = atof(str.c_str());
			cerr << "A= " << globalL << endl;
	} else {
			OUTPUT << "WARNING No A in XML file" << endl;
	}

	attr= element->Attribute("globalL");
		if (attr) {
			string str=attr;
			globalL = atof(str.c_str());
			cerr << "globalL= " << globalL << endl;
	} else {
			OUTPUT << "WARNING No globalL in XML file" << endl;
	}

	attr= element->Attribute("globalI");
					if (attr) {
						string str=attr;
						globalI = atof(str.c_str());
						cerr << "globalI= " << globalI << endl;
				} else {
						OUTPUT << "WARNING No globalI in XML file" << endl;
				}

	attr= element->Attribute("globalBeta");
					if (attr) {
						string str=attr;
						globalBeta = atof(str.c_str());
						cerr << "globalBeta= " << globalBeta << endl;
				} else {
						OUTPUT << "WARNING No globalBeta in XML file" << endl;
				}

	attr= element->Attribute("globalIz");
						if (attr) {
							string str=attr;
							globalIz = atof(str.c_str());
							cerr << "globalIz= " << globalIz << endl;
					} else {
							OUTPUT << "WARNING No globalIz in XML file" << endl;
					}

	attr= element->Attribute("globalIy");
						if (attr) {
							string str=attr;
							globalIy = atof(str.c_str());
							cerr << "globalIy= " << globalIy << endl;
					} else {
							OUTPUT << "WARNING No globalIy in XML file" << endl;
					}

	attr= element->Attribute("globalNu");
						if (attr) {
							string str=attr;
							globalNu = atof(str.c_str());
							cerr << "globalNu= " << globalNu << endl;
					} else {
							OUTPUT << "WARNING No globalNu in XML file" << endl;
					}

	attr= element->Attribute("globalJ");
						if (attr) {
							string str=attr;
							globalJ = atof(str.c_str());
							cerr << "globalJ= " << globalJ << endl;
					} else {
							OUTPUT << "WARNING No globalJ in XML file" << endl;
					}


}

void ForcesPredictionIPPTCode::SetNeighbors(){
	//set 0 for all empty neighbors
	for(int i=0;i<6;i++){
		neighbors[i][0] =0;
		neighbors[i][1] =0;
	}

	//taking neighbors and adding them to our table

	//p2p = module->getP2PNetworkInterfaceByRelPos(Cell3DPosition(0,0,1));
	P2PNetworkInterface *p2p = module->getInterface(SCLattice::Direction::Top);
	//if(p2p->getConnectedBlockBId()!=-1) { // WARNING p2p->getConnectedBlockBId returns a unsigned int ! and 0 if no block is connected
	if(p2p->getConnectedBlockBId()){
		neighbors[0][0]=p2p->getConnectedBlockBId();
	}

	//p2p = module->getP2PNetworkInterfaceByRelPos(Cell3DPosition(0,0,-1));
	p2p = module->getInterface(SCLattice::Direction::Bottom);
	//if(p2p->getConnectedBlockBId()!=-1) { // WARNING p2p->getConnectedBlockBId returns a unsigned int ! and 0 if no block is connected
	if(p2p->getConnectedBlockBId()){
		neighbors[1][0]=p2p->getConnectedBlockBId();
	}

	//P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByRelPos(Cell3DPosition(-1,0,0));
	p2p = module->getInterface(SCLattice::Direction::Left);
	//if(p2p->getConnectedBlockBId()!=-1) { // WARNING p2p->getConnectedBlockBId returns a unsigned int ! and 0 if no block is connected
	if(p2p->getConnectedBlockBId()){
		neighbors[2][0]=p2p->getConnectedBlockBId();
	}

	//p2p = module->getInterface(Cell3DPosition(1,0,0));
	p2p = module->getInterface(SCLattice::Direction::Right);
	//if(p2p->getConnectedBlockBId()!=-1) { // WARNING p2p->getConnectedBlockBId returns a unsigned int ! and 0 if no block is connected
	if(p2p->getConnectedBlockBId()){
		neighbors[3][0]=p2p->getConnectedBlockBId();
	}

	//p2p = module->getP2PNetworkInterfaceByRelPos(Cell3DPosition(0,-1,0));
	p2p = module->getInterface(SCLattice::Direction::Front);
	//if(p2p->getConnectedBlockBId()!=-1) { // WARNING p2p->getConnectedBlockBId returns a unsigned int ! and 0 if no block is connected
	if(p2p->getConnectedBlockBId()){
		neighbors[4][0]=p2p->getConnectedBlockBId();
	}

	//p2p = module->getP2PNetworkInterfaceByRelPos(Cell3DPosition(0,1,0));
	p2p = module->getInterface(SCLattice::Direction::Back);
	//if(p2p->getConnectedBlockBId()!=-1) { // WARNING p2p->getConnectedBlockBId returns a unsigned int ! and 0 if no block is connected
	if(p2p->getConnectedBlockBId()){
		neighbors[5][0]=p2p->getConnectedBlockBId();
	}

}

void ForcesPredictionIPPTCode::printNeighbors(){
	OUTPUT << "neighbors for id= " << module->blockId << ": "<< endl;
	for(int i=0;i<6;i++){
		OUTPUT<<neighbors[i][0] << ", ";
	}
	OUTPUT << endl;
	for(int i=0;i<6;i++){
			OUTPUT<<neighbors[i][1] << ", ";
	}
	OUTPUT << endl;
	OUTPUT << endl;
}


void ForcesPredictionIPPTCode::parseUserBlockElements(TiXmlElement* config) {
	cerr << "blockId=" << module->blockId << endl;

	const char *attr = config->Attribute("myAttribute");
	if (attr) {
		cerr << "myAttribute =" << attr<< endl;
	}
}


void ForcesPredictionIPPTCode::startup() {
	addMessageEventFunc(DU_MSG,_receiveMessage);


	console << "---start " << module->blockId << "," << module->color << "----\n";
	//set attributes from xml file
	mass = globalMass;
	E = globalE;
	L = globalL;
	A = globalA;
	I = globalI;

	Iz =  globalIz;
	Iy = globalIy;
	nu = globalNu;
	J = globalJ;

	grav=globalGrav;
	beta = globalBeta;


	Omega = globalOmega; // weight of Jacobi method
	Mu = globalMu; //friction coefficient
	Eps = globalEps; // //tolerance
	Gamma = globalGamma; //stiffness reduction multiplier (for unilateral contact)
	supportZ = globalSupportZ; //Z coordinate of the bottom modules (contacting with the support)


	//cheking neighbors and adding them to a list
	SetNeighbors();
	//CheckNeighbors();

	//check is module fixed
	if(isFixed(module)) {
		module->setColor(RED);
        }

	//check is modue support
	support = isSupport(module);

	//createK11(K11);
	//createK12(K12);

	//setting of the mass force vector
	Fp=orient*grav*mass*10;
	//printVector(Fp);



	//first step - calculate DU and sends to neighbor only in first
	if(curIteration == 0){
		computeDU();
		curIteration++;
	}



}

vector< vector<double> > ForcesPredictionIPPTCode::tiltingStiffnessMatrix(vector<double> &dup){
    vector< vector<double> > K11d = createK11(1); // stiffness matrix for DOWN direction
    vector< vector<double> > TfrB, TmxB, TmyB, TmzB;
    vector< double > Fd = K11d*dup;
    double fx=Fd[0], fy=Fd[1], fz=Fd[2], mx=Fd[3], my=Fd[4], mz=Fd[5]; // elastic predictor

    if(fz<0) { // contact
        double mmax = -fz*L/2;
        double fmax = -Mu*fz;

        // condition for frictional sliding
        if(sqrt(fx*fx+fy*fy)<fmax) { // frictional stick state
            TfrB = IdentityMatrix6();
        } else { // frictionaal slip state
            if(sqrt(fx*fx+fy*fy)<Eps) { // near zero tangential force -> Mu is probably very low -> stiffness matrix for sliding = 0
                TfrB = createTfr(0,0,0,0,0,0);
            } else { // frictional sliding (radial return on the Coulomb friction cone
                TfrB = createTfr(
                                -fy*fy*fz*Mu/pow(fx*fx+fy*fy,3/2),
                                fx*fy*fz*Mu/pow(fx*fx+fy*fy,3/2),
                                -fx*Mu/pow(fx*fx+fy*fy,1/2),
                                fx*fy*fz*Mu/pow(fx*fx+fy*fy,3/2),
                                -fx*fx*fz*Mu/pow(fx*fx+fy*fy,3/2),
                                -fy*Mu/pow(fx*fx+fy*fy,1/2)
                                );
                    }
                }

        // condition for x-tilting (over the y-directed edge (front or back))
        if(fabs(mx) < mmax) { // stable bending
            TmxB = IdentityMatrix6();
        } else { // unstable bending (tilting over the y-directed edge)
            if(fabs(mx)<Eps) { // near-zero torque -> fz is near zero -> stiffness matrix for x-bending will be zero
                TmxB = createTmx(0);
            } else { // tilting occurs
                TmxB = createTmx(-sign(mx)*L/2);
            }
        }

        // condition for y-tilting (over the x-directed edge (left or right))
        if(fabs(my) < mmax) { // stable bending
            TmyB = IdentityMatrix6();
        } else { // unstable bending (tilting over the y-directed edge)
            if(fabs(my)<Eps) { // near-zero torque -> fz is near zero -> stiffness matrix for x-bending will be zero
                TmyB = createTmy(0);
            } else { // tilting occurs
                TmyB = createTmy(-sign(my)*L/2);
            }
        }

        // condition for x-tilting (over the y-directed edge (front or back))
        if(fabs(mz) < mmax) { // stable bending
            TmzB = IdentityMatrix6();
        } else { // unstable bending (tilting over the y-directed edge)
            if(fabs(mz)<Eps) { // near-zero torque -> fz is near zero -> stiffness matrix for x-bending will be zero
                TmzB = createTmz(0);
            } else { // tilting occurs
                TmzB = createTmz(-sign(mz)*Mu);
            }
        }
        K11d=TfrB*TmxB*TmyB*TmzB*K11d;
    } else { // separation
        K11d=Gamma*K11d;
    }
    return K11d;
}

void ForcesPredictionIPPTCode::computeDU(){

	//temporary Matrixes
	vector< vector<double> > sumK11 = decltype(sumK11)(vectorSize, vector<double>(vectorSize,0));
	vector< double > Fpq = decltype(Fpq)(vectorSize,0);

	if(!isFixed(module)){
		//checking neighbors and creating K11 and K12 matrixes
		for(int i=0;i<6;i++){
				if(neighbors[i][0]!=0){
					//OUTPUT << module->blockId << "has neighbor" << neighbors[i] << endl;
					sumK11=sumK11+createK11(i);
					Fpq = Fpq+(createK12(i)*uq[i]);
				}

		}
		if(isSupport(module)) { // enforce the unilateral contact conditions with the support, located below the module
            sumK11=sumK11+tiltingStiffnessMatrix(dup);
		}

        du = RevD(sumK11)*beta*(Fp-createR(sumK11)*dup-Fpq)+(dup*(1-beta));

		printVector(du,6,"vector du module id= " + to_string(module->blockId) + " interaction "+ to_string(curIteration));

	}	else { //end isFixed
        du = du*0.;
	}

	//sending message to neighbors with du
	OUTPUT << "size=" << du.size() << endl;
	sendMessageToAllNeighbors("DU_MSG",new MessageOf<vector<double>>(DU_MSG,du),messageDelay,messageDelayError,0);
	//clearing info about du from neighbors
	clearNeighborsMessage();

}

void ForcesPredictionIPPTCode::visualization(){

	//calculate only if not fixed
	if(isFixed(module))
		return;

	double fxMax = 25.5*grav*mass*10; // max force in N
	double myMax = fxMax * L/2; // max moment in N*mm

	bMatrix tmpK11 = decltype(tmpK11)(vectorSize, vector<double>(vectorSize,0));
	bMatrix tmpK12 = decltype(tmpK12)(vectorSize, vector<double>(vectorSize,0));

//	bMatrix R = decltype(R)(vectorSize, vector<double>(vectorSize));


	for(int i=0;i<6;i++){
		if(neighbors[i][0]!=0 and !isFixed(module)){
			tmpK11 = createK11(i);
			tmpK12 = createK12(i);

			vizTable[i]=createRot(i)*(tmpK11*dup+tmpK12*uq[i]);

            // the torque should be averaged between the neighbours (i.e. it is taken from the middle of the beam)
			int di=1-2*(i%2);
            tmpK11 = createK11(i+di);
            tmpK12 = createK12(i+di);
            vizTable[i][4]=(vizTable[i][4]+(createRot(i+di)*(tmpK11*uq[i]+tmpK12*dup))[4])/2;
            vizTable[i][5]=(vizTable[i][5]+(createRot(i+di)*(tmpK11*uq[i]+tmpK12*dup))[5])/2;
		}
	}
	double maxS = 0;
	if(isSupport(module)) { // enforce the unilateral contact conditions with the support, located below the module
        vector<double> Fd=tiltingStiffnessMatrix(dup)*dup;
        double fx=Fd[0], fy=Fd[1], fz=Fd[2], mx=Fd[3], my=Fd[4], mz=Fd[5]; // in global coordinates
        vizTable[1][0]=fx;  vizTable[1][1]=fy;  vizTable[1][2]=fz;  vizTable[1][3]=mx;  vizTable[1][4]=my;  vizTable[1][5]=mz;
        if(fx<0) {
            maxS = min(1.,max(abs(mx)/abs(fz*L/2),abs(my)/abs(fz*L/2)));
        }
	}

	printMatrix(vizTable,6,6,"VizTable "+to_string(module->blockId));

	// chacking the maximum load factor
	for(int i = 0; i<6; i++)
	{
        if(neighbors[i][0]!=0 and !isFixed(module)) {
            if(vizTable[i][0]<0)
                vizTable[i][0] = 0;
            //set abs values of my and mz
            vizTable[i][4] = abs(vizTable[i][4]);
            vizTable[i][5] = abs(vizTable[i][5]);

            if(vizTable[i][0]>fxMax)
                vizTable[i][0] = fxMax;

            if(vizTable[i][4]>myMax)
                vizTable[i][4] = myMax;

            if(vizTable[i][5]>myMax)
                vizTable[i][5] = myMax;

            maxS=max(vizTable[i][0]/fxMax, maxS);
            maxS=max(vizTable[i][4]/myMax, maxS);
            maxS=max(vizTable[i][5]/myMax, maxS);
        }
	}
	OUTPUT << "Module " << module->blockId << " maximum load factor = "<< maxS << endl;

	//set color for module
	//cout << min(2*color,1.) << " " << min(2*(1-color),1.) << endl;
	module->setColor(Color(min(2*maxS,1.),min(2*(1-maxS),1.),0.0));

	if (curIteration==maxIterations && maxS>=1.-Eps) {
		module->setBlinkMode(true);
	}



}


void ForcesPredictionIPPTCode::receiveMessage(const MessageOf<vector<double> >*msg,P2PNetworkInterface *sender) {
	bID msgFrom = sender->getConnectedBlockBId();
	vector<double> msgData = *(msg->getData());


	if(curIteration > maxIterations)
		return;

	for(int i=0;i<6;i++){
		if(neighbors[i][0]==msgFrom){
			OUTPUT << "Iter=" << curIteration  <<  ", ID="<< module->blockId << " received the message from " << msgFrom<< endl;
			printVector(msgData,6,"msgData from "+to_string(msgFrom)+" to "+ to_string(module->blockId));
			neighbors[i][1]=1;
			uq[i]=msgData;
		}
	}
	//checking if there are all messages
	bool ready = true;
	for(int i = 0;i<6;i++ ){
		if(neighbors[i][0]!=0 && neighbors[i][1]==0)
			ready = false;
	}

	printNeighbors();

	if(ready){
		OUTPUT << "Calculating du"<< endl;
		computeDU();


		//visualisation
		if(curIteration%100==0){
			visualization();
			cout << "Current Iteration = "<< curIteration<< endl;
		}

		if(curIteration==maxIterations){
			if(support) {
				module->setColor(Color(0.0f,0.0f,1.0f));
            }
            visualization();
		}

		curIteration++;
		dup=du;
	}




}

void ForcesPredictionIPPTCode::clearNeighborsMessage() {
	for(int i=0; i<6; i++){
		neighbors[i][1]=0;
	}
}


void _receiveMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
	MessageOf<vector<double> >*msgType = (MessageOf<vector<double> >*)msg.get();
	cb->receiveMessage(msgType,sender);
}


bool ForcesPredictionIPPTCode::isFixed(BlinkyBlocksBlock *modR){
	if(target->isInTarget(modR->position)){
		modR->setColor(Color(0.8f,0.8f,0.6f));
		return true;
	}else
		return false;

}


bool ForcesPredictionIPPTCode::isSupport(BlinkyBlocksBlock *modR){
	if(modR->position[2]==supportZ){
		return true;
	}
	return false;
}


// Auxiliary functions

void ForcesPredictionIPPTCode::printVector(vector<double> &vec, int row,string desc){
	OUTPUT << "*************printVec********************"<< endl;
	OUTPUT << desc << endl;
		for (int i=0;i<row;i++){
			OUTPUT << vec[i]<< "\t";
		}
		OUTPUT <<endl;
}



void ForcesPredictionIPPTCode::printMatrix(vector< vector<double> > &matrix, int row, int col,string desc){
	OUTPUT << "*************printMatrix********************"<< endl;
	OUTPUT << desc<< endl;
	for (int i=0;i<row;i++){
		for(int j=0;j<col;j++){
			OUTPUT << matrix[i][j]<< "\t";
		}
		OUTPUT << endl;
	}
	OUTPUT << endl;

}

vector< vector<double> > ForcesPredictionIPPTCode::IdentityMatrix6(){
	vector< vector<double> > tmp = decltype(tmp)(vectorSize, vector<double>(vectorSize,0));
	//cout << "creating K11 "<<i<< endl;
	for(int k=0;k<vectorSize;k++)
			tmp[k][k]=1;
	return tmp;
}

vector< vector<double> > ForcesPredictionIPPTCode::RevD(vector< vector<double> > &A) {
	vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size(),0));
	for(int i=0;i<A.size();i++)
		tmp[i][i] = 1/A[i][i];

	return tmp;

}
vector< vector<double> > ForcesPredictionIPPTCode::createK11(int i) {
	vector< vector<double> > tmp = decltype(tmp)(vectorSize, vector<double>(vectorSize));
	//cout << "creating K11 "<<i<< endl;
	for(int k=0;k<vectorSize;k++)
		for(int m=0;m<vectorSize;m++){
			//cout << K11(i,k,m)<<"  ";
			tmp[k][m]=K11(i,k,m);
		}
	return tmp;
}

vector< vector<double> > ForcesPredictionIPPTCode::createK12(int i) {
	vector< vector<double> > tmp = decltype(tmp)(vectorSize, vector<double>(vectorSize));
		//cout << "creating K12 "<<i<< endl;
		for(int k=0;k<vectorSize;k++)
			for(int m=0;m<vectorSize;m++){
				//cout << K12(i,k,m)<<"  ";
				tmp[k][m]=K12(i,k,m);
			}
		return tmp;
}


vector< vector<double> > ForcesPredictionIPPTCode::createTfr(double Txx, double Txy, double Txz,double Tyx, double Tyy, double Tyz) {
    vector< vector<double> > t = IdentityMatrix6();
    t[0][0]=Txx;    t[0][1]=Txy;    t[0][2]=Txz;
    t[1][0]=Tyx;    t[1][1]=Tyy;    t[1][2]=Tyz;
    return t;
}

vector< vector<double> > ForcesPredictionIPPTCode::createTmx(double Txz) {
    vector< vector<double> > t = IdentityMatrix6();
    t[3][3]=0;
    t[3][2]=Txz;
    return t;
}

vector< vector<double> > ForcesPredictionIPPTCode::createTmy(double Txz) {
    vector< vector<double> > t = IdentityMatrix6();
    t[4][4]=0;
    t[4][2]=Txz;
    return t;
}

vector< vector<double> > ForcesPredictionIPPTCode::createTmz(double Txz) {
    vector< vector<double> > t = IdentityMatrix6();
    t[5][5]=0;
    t[5][2]=Txz;
    return t;
}



vector< vector<double> > ForcesPredictionIPPTCode::createRot(int i){
	vector< vector<double> > tmp = decltype(tmp)(vectorSize, vector<double>(vectorSize));
	//cout << "creating K11 "<<i<< endl;
	for(int k=0;k<vectorSize;k++)
		for(int m=0;m<vectorSize;m++){
			//cout << K11(i,k,m)<<"  ";
			tmp[k][m]=Rot(i,k,m);
		}
	return tmp;
}

vector< vector<double> > ForcesPredictionIPPTCode::createD(vector< vector<double> > &A){
    vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size(),0));
	for(int i=0; i<A.size();i++){
				tmp[i][i] = A[i][i]; // operation
	}
	return tmp;
}
vector< vector<double> > ForcesPredictionIPPTCode::createR(vector< vector<double> > &A){

	vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size(),0));

	for(int i=0; i<A.size();i++) {
		for(int j=0; j<A[0].size();j++) {
            if(i!=j) {
				tmp[i][j] = A[i][j];
            }
		}// operation
	}
	return tmp;
}


void vector2string(const std::vector<bID>&v,string &s) {
	vector<bID>::const_iterator it = v.begin();
	s.clear();
	while (it!=v.end()) {
		s+= to_string(*it) + ",";
		it++;
	}
}


//OPERATORS

// vec * scal
vector<double> operator*(const vector<double> vec, const double  scal){
	vector<double> tmp = decltype(tmp)(vec.size(),0);
	//cout << vec.size();
		for (int i=0;i<vec.size();i++){
			tmp[i] = vec[i]*scal;
		}
	return tmp;
}

// scal * vec
vector<double> operator*(const double  scal, const vector<double> vec){
	vector<double> tmp = decltype(tmp)(vec.size(),0);
	//cout << vec.size();
		for (int i=0;i<vec.size();i++){
			tmp[i] = vec[i]*scal;
		}
	return tmp;
}

// vec + vec
vector<double> operator+(const vector<double> vec1, const vector<double> vec2){
	size_t smax = max(vec1.size(),vec2.size());
	vector<double> tmp = decltype(tmp)(smax,0);
	if (vec1.size()<vec2.size()) {
		for (size_t i=0;i<vec1.size();i++){
				tmp[i] = vec1[i]+vec2[i];
		}
		for (size_t i=vec1.size();i<vec2.size();i++){
				tmp[i] = vec2[i];
		}
	} else {
		for (size_t i=0;i<vec2.size(); i++) {
				tmp[i] = vec1[i]+vec2[i];
		}
		for (size_t i=vec2.size();i<vec1.size();i++) {
				tmp[i] = vec1[i];
		}
	}
	return tmp;
}

// vec - vec
vector<double> operator-(const vector<double> vec1, const vector<double> vec2) {
	size_t smax = max(vec1.size(),vec2.size());
	vector<double> tmp = decltype(tmp)(smax,0);
	if (vec1.size()<vec2.size()) {
		for (size_t i=0;i<vec1.size();i++){
				tmp[i] = vec1[i]-vec2[i];
		}
		for (size_t i=vec1.size();i<vec2.size();i++){
				tmp[i] = -vec2[i];
		}
	} else {
		for (size_t i=0;i<vec2.size();i++){
				tmp[i] = vec1[i]-vec2[i];
		}
		for (size_t i=vec2.size();i<vec1.size();i++){
				tmp[i] = vec1[i];
		}
	}
	return tmp;
}

// - vec
vector<double> operator-(const vector<double> vec){
	vector<double> tmp = decltype(tmp)(vec.size(),0);
	for (size_t i=0;i<vec.size();i++)
		tmp[i] = -vec[i];
	return tmp;
}

// mat * vec
vector<double>  operator*(const vector< vector<double> > A, const vector<double> vec){
	vector<double> tmp = decltype(tmp)(A.size(),0);
	for (int i=0;i<A.size();i++){
	        for (int j=0;j<vec.size();j++){
	            tmp[i]+=( A[i][j]*vec[j]);
	        }
	    }
	return tmp;
}

// mat * scal
vector< vector<double> > operator*(const vector< vector<double> > A, const double B){
	vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size()));
	for(int i=0; i<A.size();i++){
			for(int j=0;j<A[0].size();j++)	{
				tmp[i][j] = A[i][j] * B;
			}
	}
	return tmp;
}

// scal * mat
vector< vector<double> > operator*(const double B, const vector< vector<double> > A){
	vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size()));
	for(int i=0; i<A.size();i++){
			for(int j=0;j<A[0].size();j++)	{
				tmp[i][j] = A[i][j] * B;
			}
	}
	return tmp;
}

// mat * mat
vector< vector<double> > operator*(const vector< vector<double> > A,const vector< vector<double> > B){
	vector< vector<double> > result = decltype(result)(A.size(), vector<double>(B[0].size()));
	for(int i=0; i<A.size();i++){
		for(int j=0;j<B[0].size();j++)	{
			for(int k=0;k<B.size();k++){
				result[i][j] += A[i][k] * B[k][j]; // operation
			}
		}
	}
	return result;
}

// mat + mat
vector< vector<double> > operator+(vector< vector<double> > A,vector< vector<double> > B) {
	vector< vector<double> > result = decltype(result)(A.size(), vector<double>(A[0].size()));
	for(int i=0; i<A.size();i++){
			for(int j=0;j<A[0].size();j++)	{
					result[i][j] = A[i][j] + B[i][j]; // operation
			}
		}
	return result;
}

// mat - mat
vector< vector<double> > operator-(vector< vector<double> > A,vector< vector<double> > B) {
	vector< vector<double> > result = decltype(result)(A.size(), vector<double>(A[0].size()));
	for(int i=0; i<A.size();i++){
			for(int j=0;j<A[0].size();j++)	{
					result[i][j] = A[i][j] - B[i][j]; // operation
			}
		}
	return result;
}

// - mat
vector< vector<double> > operator-(vector< vector<double> > A) {
	vector< vector<double> > result = decltype(result)(A.size(), vector<double>(A[0].size()));
	for(int i=0; i<A.size();i++){
			for(int j=0;j<A[0].size();j++)	{
					result[i][j] = -A[i][j]; // operation
			}
		}
	return result;
}


