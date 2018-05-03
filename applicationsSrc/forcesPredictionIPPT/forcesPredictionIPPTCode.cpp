#include "forcesPredictionIPPTCode.h"

const int messageDelay=50;
const int messageDelayError=5;
const int messageDelayCons=1;


int maxIterations = 2; // max number of iterations
double globalMass = 0; //mass from XML
double globalE = 100; // E from XML // Young modulus MPa
double globalL=40; //length from XML // arm length mm
double globala = 40; //width of the square-cross-section arm  mm //
double globalA=globala*globala; //cross sectional area from XML mm^2
double globalI=pow(globala,4)/12.; // second moment of area from XML mm^4
double globalIz = globalI; //second moment of area
double globalIy = globalI; //second moment of area
double globalNu = 0.3; //Poisson ratio
double globalJ=1; //torsion constant

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
		globala = atoi(str.c_str());
		cerr << "a= " << globala << endl;
	} else {
			OUTPUT << "WARNING No a in XML file" << endl;
	}


	attr= element->Attribute("E");
		if (attr) {
			string str=attr;
			globalE = atoi(str.c_str());
			cerr << "E= " << globalE << endl;
	} else {
			OUTPUT << "WARNING No E in XML file" << endl;
	}

	attr= element->Attribute("A");
		if (attr) {
			string str=attr;
			globalA = atoi(str.c_str());
			cerr << "A= " << globalL << endl;
	} else {
			OUTPUT << "WARNING No A in XML file" << endl;
	}

	attr= element->Attribute("globalL");
		if (attr) {
			string str=attr;
			globalL = atoi(str.c_str());
			cerr << "globalL= " << globalL << endl;
	} else {
			OUTPUT << "WARNING No globalL in XML file" << endl;
	}

	attr= element->Attribute("globalI");
					if (attr) {
						string str=attr;
						globalI = atoi(str.c_str());
						cerr << "globalI= " << globalI << endl;
				} else {
						OUTPUT << "WARNING No globalI in XML file" << endl;
				}

	attr= element->Attribute("globalBeta");
					if (attr) {
						string str=attr;
						globalBeta = atoi(str.c_str());
						cerr << "globalBeta= " << globalBeta << endl;
				} else {
						OUTPUT << "WARNING No globalBeta in XML file" << endl;
				}

	attr= element->Attribute("globalIz");
						if (attr) {
							string str=attr;
							globalIz = atoi(str.c_str());
							cerr << "globalIz= " << globalIz << endl;
					} else {
							OUTPUT << "WARNING No globalIz in XML file" << endl;
					}

	attr= element->Attribute("globalIy");
						if (attr) {
							string str=attr;
							globalIy = atoi(str.c_str());
							cerr << "globalIy= " << globalIy << endl;
					} else {
							OUTPUT << "WARNING No globalIy in XML file" << endl;
					}

	attr= element->Attribute("globalNu");
						if (attr) {
							string str=attr;
							globalNu = atoi(str.c_str());
							cerr << "globalNu= " << globalNu << endl;
					} else {
							OUTPUT << "WARNING No globalNu in XML file" << endl;
					}

	attr= element->Attribute("globalJ");
						if (attr) {
							string str=attr;
							globalJ = atoi(str.c_str());
							cerr << "globalJ= " << globalJ << endl;
					} else {
							OUTPUT << "WARNING No globalJ in XML file" << endl;
					}


}


void ForcesPredictionIPPTCode::visualization(){

	double fxMax = 25.5*61; // max force
	double fxMmax = fxMax * 20; // max moment

	bMatrix tmpK11 = decltype(tmpK11)(vectorSize, vector<double>(vectorSize));
	bMatrix tmpK12 = decltype(tmpK12)(vectorSize, vector<double>(vectorSize));

	bMatrix R = decltype(R)(vectorSize, vector<double>(vectorSize));

	//creating K11 and K12
	for(int i=0;i<6;i++){
			if(neighbors[i][0]!=0){
				//OUTPUT << module->blockId << "has neighbor" << neighbors[i] << endl;
				tmpK11=tmpK11+createK11(i);
				tmpK12 = tmpK12+createK12(i);
			}
	}

	for(int i=0;i<6;i++){

		//vizTable[i]=R*tmpK11*u+R*tmpK12*uq;

	}

}

void ForcesPredictionIPPTCode::parseUserBlockElements(TiXmlElement* config) {
	cerr << "blockId=" << module->blockId << endl;
	
	const char *attr = config->Attribute("myAttribute");
	if (attr) {
		cerr << "myAttribute =" << attr<< endl;
	}
}


void ForcesPredictionIPPTCode::startup() {
	addMessageEventFunc(DU_MSG,_ProcSendDuFunc);

	
	console << "start " << module->blockId << "," << module->color << "\n";
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
	if(isFixed(module))
		module->setColor(RED);

	//check is modue support
	support = isSupport(module);

	//createK11(K11);
	//createK12(K12);

	//setting of the Fp force
	Fp=orient*grav;
	printVector(Fp);



	//first step - calculate DU and sends to neighbor
	calculateU();

	


}

bool ForcesPredictionIPPTCode::isSupport(BlinkyBlocksBlock *modR){
	if(modR->position[2]==supportZ){
		modR->setColor(Color(0.8f,0.8f,0.6f));
		return true;
	}
}

void ForcesPredictionIPPTCode::calculateU(){

	//temporary Matrixes
	bMatrix tmpK11 = decltype(tmpK11)(vectorSize, vector<double>(vectorSize));
	vector< double > tmpK12 = decltype(tmpK12)(vectorSize,0);

	if(!isFixed(module)){
		//checking neighbors and creating K11 and K12 matrixes
		for(int i=0;i<6;i++){
				if(neighbors[i][0]!=0){
					//OUTPUT << module->blockId << "has neighbor" << neighbors[i] << endl;
					tmpK11=tmpK11+createK11(i);
					tmpK12 = tmpK12+(createK12(i)*uq[i]);
				}

		}

		printMatrix(tmpK11);
		printVector(tmpK12,6);


		//creating R and D
		bMatrix R = decltype(R)(vectorSize, vector<double>(vectorSize)); //R vector
		bMatrix D = decltype(D)(vectorSize, vector<double>(vectorSize)); //D vector
		bMatrix revD = decltype(revD)(vectorSize, vector<double>(vectorSize)); //revD vector

		createD(tmpK11,D);
		createR(tmpK11,R);
		createRevD(D,revD);

		//printMatrix(tmpK11);

		vector<double> tmp = decltype(tmp)(vectorSize,0); //tmp u
		vector<double> tmp1 = decltype(tmp)(vectorSize,0);
		bMatrix  tmpBD = decltype(tmpBD)(vectorSize, vector<double>(vectorSize));

		//Beta * revD
		tmpBD = revD*beta;
		//Fp - fp
		tmp = Fp+(fp*-1.);

		//add Ru part
		tmp1 = R*u;
		tmp1 = tmp1*-1.;
		tmp = tmp+tmp1;

		// add K12 part
		tmp1 = tmpK12*-1.;
		tmp = tmp+tmp1;

		//bd * ()
		tmp = tmpBD*tmp;

		//bd*()-(1-B)u-1
		tmp=tmp+(u*(1-beta));

		du=tmp;



		//printVector(u);
	}	else { //end isFixed
		du[0] = 0;
		du[1] = 0;
		du[2] = 0;
	}

	//sending message to neighbors with du
	sendMessageToAllNeighbors("DU_MSG",new MessageOf<vector<double> >(DU_MSG,du),messageDelay,messageDelayError,0);
	//clearing info about du from neighbors
	clearNeighborsMessage();



}

void ForcesPredictionIPPTCode::SetNeighbors(){
	//set 0 for all empty neighbors
	for(int i=0;i<6;i++){
		neighbors[i][0] =0;
		neighbors[i][1] =0;
	}

	//taking neighbors and adding them to our table

	//P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByRelPos(Cell3DPosition(-1,0,0));
	P2PNetworkInterface *p2p = module->getInterface(SCLattice::Direction::Left);
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

	//p2p = module->getP2PNetworkInterfaceByRelPos(Cell3DPosition(0,0,1));
	p2p = module->getInterface(SCLattice::Direction::Top);
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

}

void ForcesPredictionIPPTCode::CheckNeighbors(){
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

bool ForcesPredictionIPPTCode::isFixed(BlinkyBlocksBlock *modR){
	/*if(target->isInTarget(modR->position)){
		return true;
	}else*/
		return false;

}


void ForcesPredictionIPPTCode::ProcSendDuFunc(const MessageOf<vector<double> >*msg,P2PNetworkInterface *sender) {
	bID msgFrom = sender->getConnectedBlockBId();
	vector<double> msgData = *(msg->getData());
	

	if(curIteration > maxIterations)
		return;

	for(int i=0;i<6;i++){
		if(neighbors[i][0]==msgFrom){
			OUTPUT << "Iter=" << curIteration  <<  ", ID="<< module->blockId << " received the message from " << msgFrom<< endl;
			printVector(msgData);
			neighbors[i][1]=1;
			uq[i]=msgData;
		}
	}
	//checking if there are all messages
	bool calculateDu = true;
	for(int i = 0;i<6;i++ ){
		if(neighbors[i][0]!=0 && neighbors[i][1]==0)
			calculateDu = false;
	}
	
	CheckNeighbors();

	if(calculateDu){
		OUTPUT << "Calculating du"<< endl;
		calculateU();
		curIteration++;

	}

	//visualisation
	visualization();

	
}

void ForcesPredictionIPPTCode::clearNeighborsMessage(){
	for(int i =0;i<6;i++){
		neighbors[i][1]=0;
	}
}
void _ProcSendDuFunc(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
	ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
	MessageOf<vector<double> >*msgType = (MessageOf<vector<double> >*)msg.get();
	cb->ProcSendDuFunc(msgType,sender);
}

Vector3D ForcesPredictionIPPTCode::toVec3D(vector<double> vec1){
	Vector3D tmp{0,0,0};
	Vector3D *wsk = &tmp;
	for(int i=0;i<3;i++){
		OUTPUT << *wsk;
	}
	return tmp;
}

void ForcesPredictionIPPTCode::printVector(vector<double> &vec, int row,string desc){
	OUTPUT << "*************printVec********************"<< endl;
	OUTPUT << desc;
		for (int i=0;i<row;i++){
			OUTPUT << vec[i]<< "\t";
		}
		OUTPUT <<endl;
}



void ForcesPredictionIPPTCode::printMatrix(vector< vector<double> > &matrix, int row, int col,string desc){
	OUTPUT << "*************printMatrix********************"<< endl;
	OUTPUT << desc;
	for (int i=0;i<row;i++){
		for(int j=0;j<col;j++){
			OUTPUT << matrix[i][j]<< "\t";
		}
		OUTPUT << endl;
	}
	OUTPUT << endl;

}

void ForcesPredictionIPPTCode::createRevD(vector< vector<double> > &matrix, vector< vector<double> > &result){

	vector< vector<double> > tmp = decltype(tmp)(matrix.size(), vector<double>(matrix.size()));
	for(int i=0;i<matrix.size();i++)
		tmp[i][i] = 1/matrix[i][i];

	result = tmp;

}
vector< vector<double> > ForcesPredictionIPPTCode::createK11(int i){
	vector< vector<double> > tmp = decltype(tmp)(vectorSize, vector<double>(vectorSize));
	//cout << "creating K11 "<<i<< endl;
	for(int k=0;k<vectorSize;k++)
		for(int m=0;m<vectorSize;m++){
			//cout << K11(i,k,m)<<"  ";
			tmp[k][m]=K11(i,k,m);
		}
	return tmp;
}

vector< vector<double> > ForcesPredictionIPPTCode::createK12(int i){
	vector< vector<double> > tmp = decltype(tmp)(vectorSize, vector<double>(vectorSize));
		//cout << "creating K12 "<<i<< endl;
		for(int k=0;k<vectorSize;k++)
			for(int m=0;m<vectorSize;m++){
				//cout << K12(i,k,m)<<"  ";
				tmp[k][m]=K12(i,k,m);
			}
		return tmp;
}

void ForcesPredictionIPPTCode::createD(vector< vector<double> > &A, vector< vector<double> > &result){

	for(int i=0; i<A.size();i++){
				result[i][i] = A[i][i]; // operation
	}
}
void ForcesPredictionIPPTCode::createR(vector< vector<double> > &A, vector< vector<double> > &result){

	vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A.size()));
	createD(A,tmp);

	for(int i=0; i<A.size();i++){
		for(int j=0; j<A.size();j++){
				result[i][j] =A[i][j]- tmp[i][j];
		}// operation
	}
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

vector<double> operator*(const vector<double> vec, const double  scal){
	vector<double> tmp = decltype(tmp)(vec.size(),0);
	cout << vec.size();
		for (int i=0;i<vec.size();i++){
			tmp[i] = vec[i]*scal;
		}
	return tmp;
}
vector<double> operator+(const vector<double> vec1 ,const vector<double> vec2){
	vector<double> tmp = decltype(tmp)(3,0);
	for (int i=0;i<3;i++){
				tmp[i] = vec1[i]+vec2[i];
		}
	return tmp;
}

vector<double>  operator*(const vector< vector<double> > A, const vector<double> vec){
	vector<double> tmp = decltype(tmp)(vec.size(),0);
	for (int i=0;i<vec.size();i++){
	        for (int j=0;j<vec.size();j++){
	            tmp[i]+=( A[i][j]*vec[j]);
	        }
	    }
	return tmp;
}
vector< vector<double> > operator*(const vector< vector<double> > A,const double B){
	vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A.size()));
	for(int i=0; i<A.size();i++){
			for(int j=0;j<A.size();j++)	{
				tmp[i][j] = A[i][j] * B;
			}
	}
	return tmp;
}

vector< vector<double> > operator*(const vector< vector<double> > A,const vector< vector<double> > B){
	vector< vector<double> > result = decltype(result)(A.size(), vector<double>(A.size()));
	for(int i=0; i<A.size();i++){
		for(int j=0;j<A.size();j++)	{
			for(int k=0;k<B.size();k++){
				result[i][j] += A[i][k] * B[k][j]; // operation
			}
		}
	}
	return result;
}

vector< vector<double> > operator+(vector< vector<double> > A,vector< vector<double> > B){
	vector< vector<double> > result = decltype(result)(A.size(), vector<double>(A.size()));
	for(int i=0; i<A.size();i++){
			for(int j=0;j<A.size();j++)	{
					result[i][j] = A[i][j] + B[i][j]; // operation
			}
		}
	return result;
}


