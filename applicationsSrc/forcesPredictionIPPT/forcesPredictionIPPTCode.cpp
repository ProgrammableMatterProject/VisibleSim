#include "forcesPredictionIPPTCode.h"

const int messageDelay=50;
const int messageDelayError=5;
const int messageDelayCons=1;


int maxIterations = 2; // max number of iterations
double globalMass = 0; //mass from XML
double globalE = 0; // E from XML
double globalL=4; //length from XML
double globalA=1; //cross sectional area from XML
double globalI=1/12.; //area from XML


double globalGrav=9.81; //gravity from XML
double globalBeta=2/3.; //beta from XML

/* parse XML files extra data */
/* be carefull, is run only one time by the first module! */
void ForcesPredictionIPPTCode::parseUserElements(TiXmlDocument* config) {
	TiXmlNode *node = config->FirstChild("parameters");
	
	cerr << "blockId=" << module->blockId << endl;
	TiXmlElement* element = node->ToElement();
	const char *attr= element->Attribute("globalMass");
	//mass of module
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
				globalL = atoi(str.c_str());
				cerr << "A= " << globalL << endl;
		} else {
				OUTPUT << "WARNING No A in XML file" << endl;
		}

	attr= element->Attribute("L");
				if (attr) {
					string str=attr;
					globalL = atoi(str.c_str());
					cerr << "L= " << globalA << endl;
			} else {
					OUTPUT << "WARNING No L in XML file" << endl;
			}

	attr= element->Attribute("I");
					if (attr) {
						string str=attr;
						globalL = atoi(str.c_str());
						cerr << "I= " << globalA << endl;
				} else {
						OUTPUT << "WARNING No I in XML file" << endl;
				}
	attr= element->Attribute("Beta");
					if (attr) {
						string str=attr;
						globalBeta = atoi(str.c_str());
						cerr << "I= " << globalBeta << endl;
				} else {
						OUTPUT << "WARNING No Beta in XML file" << endl;
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
	grav=globalGrav;
	beta = globalBeta;


	cout << K111(1,1) << "dddddddddddddddddddddddddddd"<< endl<< endl;
	
	//cheking neighbors and adding them to a list
	SetNeighbors();
	//CheckNeighbors();
	
	//check is module fixed
	if(isFixed(module))
		module->setColor(RED);


	createK11(K11);
	createK12(K12);

	//setting of the Fp force
	Fp=orient*grav;
	printVector(Fp);



	//first step - calculate DU and sends to neighbor
	calculateU();

	


}

void ForcesPredictionIPPTCode::calculateU(){

	//temporary Matrixes
	bMatrix tmpK11 = decltype(tmpK11)(3, vector<double>(3));
	vector< double > tmpK12 = decltype(tmpK12)(3,0);

	if(!isFixed(module)){
		//checking neighbors and creating K11 and K12 matrixes
		for(int i=0;i<6;i++){
				if(neighbors[i][0]!=0){
					//cout << module->blockId << "has neighbor" << neighbors[i] << endl;
					tmpK11 = tmpK11+K11[i];
					tmpK12 = tmpK12+(K12[i]*uq[i]);
				}

		}
		//creating R and D
		bMatrix R = decltype(R)(3, vector<double>(3)); //R vector
		bMatrix D = decltype(D)(3, vector<double>(3)); //D vector
		bMatrix revD = decltype(revD)(3, vector<double>(3)); //revD vector

		createD(tmpK11,D);
		createR(tmpK11,R);
		createRevD(D,revD);

		//printMatrix(tmpK11);

		vector<double> tmp = decltype(tmp)(3,0); //tmp u
		vector<double> tmp1 = decltype(tmp)(3,0);
		bMatrix  tmpBD = decltype(tmpBD)(3, vector<double>(3));

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
	cout << "neighbors for id= " << module->blockId << ": "<< endl;
	for(int i=0;i<6;i++){
		cout<<neighbors[i][0] << ", ";
	}
	cout << endl;
	for(int i=0;i<6;i++){
			cout<<neighbors[i][1] << ", ";
	}
	cout << endl;
	cout << endl;
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
			cout << "Iter=" << curIteration  <<  ", ID="<< module->blockId << " received the message from " << msgFrom<< endl;
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
		cout << "Calculating du"<< endl;
		calculateU();
		curIteration++;

	}


	
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
		cout << *wsk;
	}
	return tmp;
}

void ForcesPredictionIPPTCode::printVector(vector<double> &vec, int row){
//	cout << "*************printVec********************"<< endl;
		for (int i=0;i<row;i++){
			cout << vec[i]<< "\t";
		}
		cout <<endl;
}



void ForcesPredictionIPPTCode::printMatrix(vector< vector<double> > &matrix, int row, int col){
	cout << "*************printMatrix********************"<< endl;
	for (int i=0;i<row;i++){
		for(int j=0;j<col;j++){
			cout << matrix[i][j]<< "\t";
		}
		cout << endl;
	}
	cout << endl;

}

void ForcesPredictionIPPTCode::createRevD(vector< vector<double> > &matrix, vector< vector<double> > &result){

	vector< vector<double> > tmp = decltype(tmp)(matrix.size(), vector<double>(matrix.size()));
	for(int i=0;i<matrix.size();i++)
		tmp[i][i] = 1/matrix[i][i];

	result = tmp;

}
void ForcesPredictionIPPTCode::createK11(vector<bMatrix> &matrix){

	//upper neighbor
	matrix[0][0][0] = (12*E*I)/(pow(L,3));
	matrix[0][0][2] = (-6*E*I)/(pow(L,2));
	matrix[0][1][1] = (A*E)/L;
	matrix[0][2][0] = (-6*E*I)/(pow(L,2));
	matrix[0][2][2] = (4*E*I)/L;

	//down neighbor
	matrix[1][0][0] = (12*E*I)/(pow(L,3));
	matrix[1][0][2] = (6*E*I)/(pow(L,2));
	matrix[1][1][1] = (A*E)/L;
	matrix[1][2][0] = (6*E*I)/(pow(L,2));
	matrix[1][2][2] = (4*E*I)/L;

	//left neighbor
	matrix[2][0][0] = (A*E)/L;
	matrix[2][1][1] = (12*E*I)/(pow(L,3));
	matrix[2][1][2] = (-6*E*I)/(pow(L,2));
	matrix[2][2][1] = (-6*E*I)/(pow(L,2));
	matrix[2][2][2] = (4*E*I)/L;

	//right neighbor
	matrix[3][0][0] = (A*E)/L;
	matrix[3][1][1] = (12*E*I)/(pow(L,3));
	matrix[3][1][2] = (6*E*I)/(pow(L,2));
	matrix[3][2][1] = (6*E*I)/(pow(L,2));
	matrix[3][2][2] = (4*E*I)/L;
}

void ForcesPredictionIPPTCode::createK12(vector<bMatrix> &matrix){
	//upper neighbor
	matrix[0][0][0] = (-12*E*I)/(pow(L,3));
	matrix[0][0][2] = (-6*E*I)/(pow(L,2));
	matrix[0][1][1] = (-A*E)/L;
	matrix[0][2][0] = (6*E*I)/(pow(L,2));
	matrix[0][2][2] = (2*E*I)/L;

	//down neighbor
	matrix[1][0][0] = (-12*E*I)/(pow(L,3));
	matrix[1][0][2] = (6*E*I)/(pow(L,2));
	matrix[1][1][1] = (-A*E)/L;
	matrix[1][2][0] = (-6*E*I)/(pow(L,2));
	matrix[1][2][2] = (2*E*I)/L;

	//left neighbor
	matrix[2][0][0] = (-A*E)/L;
	matrix[2][1][1] = (-12*E*I)/(pow(L,3));
	matrix[2][1][2] = (-6*E*I)/(pow(L,2));
	matrix[2][2][1] = (6*E*I)/(pow(L,2));
	matrix[2][2][2] = (2*E*I)/L;

	//right neighbor
	matrix[3][0][0] = (-A*E)/L;
	matrix[3][1][1] = (-12*E*I)/(pow(L,3));
	matrix[3][1][2] = (6*E*I)/(pow(L,2));
	matrix[3][2][1] = (-6*E*I)/(pow(L,2));
	matrix[3][2][2] = (2*E*I)/L;
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
	vector<double> tmp = decltype(tmp)(3,0);
		for (int i=0;i<3;i++){
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


