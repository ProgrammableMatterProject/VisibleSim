#ifndef forcesPredictionIPPTCode_H_
#define forcesPredictionIPPTCode_H_
#include "robotBlocksSimulator.h"
#include "robotBlocksBlockCode.h"

static const int DU_MSG=1001;

//static const int CONFIRM_EDGE_MSG=1002;
//static const int CUT_OFF_MSG=1003;
//static const int AVAILABLE_MSG=1004;
//static const int CONFIRM_PATH_MSG=1005;
//static const int CONFIRM_STREAMLINE_MSG=1005;

using namespace RobotBlocks;

//enum PathState {NONE, BFS, ConfPath, Streamline};

class ForcesPredictionIPPTCode : public RobotBlocksBlockCode {


private:
	RobotBlocksBlock *module;

	double E=1; // elastic modulus
 	double L=4; //length
	double A=1; //cross sectional area
	double I=1/12.; //area


	double mass=1; //mass
	double grav=9.81; //gravity
	double beta=2/3.; //beta
	vector<double> orient={0,0,-1};

	int curIteration = 0; // current iteration


	typedef vector< vector<double> > bMatrix;

	vector<bMatrix> K11 = decltype(K11)(6,vector<vector<double> >(3,vector <double>(3,0))); //K11 vector
	vector<bMatrix> K12 = decltype(K12)(6,vector<vector<double> >(3,vector <double>(3,0))); //K12 vector



	vector <double> u = decltype(u)(3,0); // vector u
	bMatrix uq = decltype(uq)(6, vector<double>(3,0)); //vector u from -1 step neighbors

	vector <double> du = decltype(du)(3,0);

	vector <double> fp = decltype(fp)(3,0);
	vector <double> Fp = decltype(Fp)(3,0);



	bID neighbors[6][2];// Neighbors 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1 ///// second row  if tehre is a message with du


public :
	ForcesPredictionIPPTCode(RobotBlocksBlock *host):RobotBlocksBlockCode(host) { module=host; };
	~ForcesPredictionIPPTCode() {};

	void startup();
	
	void SetNeighbors();
	void CheckNeighbors();

	void calculateU();

	bool isFixed(RobotBlocksBlock *modR);

	void printMatrix(vector< vector<double> > &matrix, int row=3, int col=3);
	void printVector(vector<double> &vec, int row=3);
	void clearNeighborsMessage(); //function to clear messages when calculated u

	void createK11(vector<bMatrix> &matrix);
	void createK12(vector<bMatrix> &matrix);

	void createR(vector< vector<double> > &A, vector< vector<double> > &result);
	void createD(vector< vector<double> > &A, vector< vector<double> > &result);
	void createRevD(vector< vector<double> > &matrix, vector< vector<double> > &result);

	vector<double> multiVecScal(vector<double> vec ,double  scal);
	vector<double> addVec(vector<double> vec1 ,vector<double> vec2);
	Vector3D toVec3D(vector<double> vec1);

	vector<double> multiMatVec(vector< vector<double> > A, vector<double> vec);
	vector< vector<double> > multiMatScal(vector< vector<double> > A,double B);
	vector< vector<double> > multiMat(vector< vector<double> > A,vector< vector<double> > B);
	vector< vector<double> > addMat(vector< vector<double> > A,vector< vector<double> > B);

	void ProcSendDuFunc(const MessageOf<vector<double> >*msg,P2PNetworkInterface *sender);

	void parseUserElements(TiXmlDocument* config);
	
/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
	    return(new ForcesPredictionIPPTCode((RobotBlocksBlock*)host));
	};
/*****************************************************************************/
};

void _ProcSendDuFunc(BlockCode*,MessagePtr,P2PNetworkInterface *sender);


void vector2string(const std::vector<bID>&v,string &s);
#endif /* forcesPredictionIPPTCode_H_ */
