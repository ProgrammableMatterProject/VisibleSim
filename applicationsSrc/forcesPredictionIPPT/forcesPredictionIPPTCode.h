#ifndef forcesPredictionIPPTCode_H_
#define forcesPredictionIPPTCode_H_
#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksBlockCode.h"
#include "kMatrixes.h"
static const int DU_MSG=1001;

//static const int CONFIRM_EDGE_MSG=1002;
//static const int CUT_OFF_MSG=1003;
//static const int AVAILABLE_MSG=1004;
//static const int CONFIRM_PATH_MSG=1005;
//static const int CONFIRM_STREAMLINE_MSG=1005;


#define vectorSize 6

using namespace BlinkyBlocks;

//enum PathState {NONE, BFS, ConfPath, Streamline};

class ForcesPredictionIPPTCode : public BlinkyBlocksBlockCode {


private:
	BlinkyBlocksBlock *module;

	double E; // elastic modulus
 	double L; //length
	double A; //cross sectional area
	double I; //area
	double Iz; //second moment of area
	double Iy; //second moment of area
	double nu; //Poisson ratio
	double J; //torsion constant

	double mass; //mass
	double grav; //gravity
	double beta; //beta

	double Omega; // weight of Jacobi method
	double Mu; //friction coefficient
	double Eps; // //tolerance
	double Gamma; //stiffness reduction multiplier (for unilateral contact)
	double supportZ; //Z coordinate of the bottom modules (contacting with the support)

	bool support = false;

	vector<double> orient={0,0,-1};

	int curIteration = 0; // current iteration


	typedef vector< vector<double> > bMatrix;

	//vector<bMatrix> K11 = decltype(K11)(6,vector<vector<double> >(3,vector <double>(3,0))); //K11 vector
	//vector<bMatrix> K12 = decltype(K12)(6,vector<vector<double> >(3,vector <double>(3,0))); //K12 vector



	vector <double> u = decltype(u)(vectorSize,0); // vector u
	bMatrix uq = decltype(uq)(6, vector<double>(vectorSize,0)); //vector u from -1 step neighbors

	vector <double> du = decltype(du)(vectorSize,0);

	vector <double> fp = decltype(fp)(3,0);
	vector <double> Fp = decltype(Fp)(3,0);



	bID neighbors[6][2];// Neighbors 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1 ///// second row  if tehre is a message with du


public :
	ForcesPredictionIPPTCode(BlinkyBlocksBlock *host):BlinkyBlocksBlockCode(host) { module=host; };
	~ForcesPredictionIPPTCode() {};

	void startup();
	
	void SetNeighbors();
	void CheckNeighbors();

	void calculateU();

	bool isFixed(BlinkyBlocksBlock *modR);

	bool isSupport(BlinkyBlocksBlock *modR);

	void printMatrix(vector< vector<double> > &matrix, int row=vectorSize, int col=vectorSize);
	void printVector(vector<double> &vec, int row=3);
	void clearNeighborsMessage(); //function to clear messages when calculated u

	vector< vector<double> > createK11(int i);
	vector< vector<double> > createK12(int i);

	void createR(vector< vector<double> > &A, vector< vector<double> > &result);
	void createD(vector< vector<double> > &A, vector< vector<double> > &result);
	void createRevD(vector< vector<double> > &matrix, vector< vector<double> > &result);

	Vector3D toVec3D(vector<double> vec1);

	void ProcSendDuFunc(const MessageOf<vector<double> >*msg,P2PNetworkInterface *sender);

	void parseUserElements(TiXmlDocument* config);
	void parseUserBlockElements(TiXmlElement* config);	

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
	    return(new ForcesPredictionIPPTCode((BlinkyBlocksBlock*)host));
	};
/*****************************************************************************/
};
//OPERATORS

vector<double> operator*(const vector<double> vec ,const double  scal);
vector<double> operator+(const vector<double> vec1 ,const vector<double> vec2);
vector<double> operator*(const vector< vector<double> > A, const vector<double> vec);
vector< vector<double> > operator*(const vector< vector<double> > A,const double B);
vector< vector<double> > operator*(const vector< vector<double> > A,const vector< vector<double> > B);
vector< vector<double> > operator+(const vector< vector<double> > A,const vector< vector<double> > B);

void _ProcSendDuFunc(BlockCode*,MessagePtr,P2PNetworkInterface *sender);


void vector2string(const std::vector<bID>&v,string &s);
#endif /* forcesPredictionIPPTCode_H_ */
