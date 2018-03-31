#ifndef forcesPredictionIPPTCode_H_
#define forcesPredictionIPPTCode_H_
#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksBlockCode.h"

static const int DU_MSG=1001;

//static const int CONFIRM_EDGE_MSG=1002;
//static const int CUT_OFF_MSG=1003;
//static const int AVAILABLE_MSG=1004;
//static const int CONFIRM_PATH_MSG=1005;
//static const int CONFIRM_STREAMLINE_MSG=1005;

//0 -z+1 1 z-1

#define K111(i,j,k)((const double[4][6][6]) {\
{\
	{12.*E*Iy/(L*L*L), 0, 0, 0, 6.*E*Iy/(L*L), 0},\
	{0, 12.*E*Iz/(L*L*L), 0, -6*E*Iz/(L*L), 0, 0},\
	{0, 0, A*E/L, 0, 0, 0},\
	{0, -6*E*Iz/(L*L), 0 , 4*E*Iz/L, 0, 0},\
	{6*E*Iy/(L*L), 0, 0, 0, 4*E*Iy/L, 0},\
	{0, 0, 0, 0, 0, E*J/(2*L*(1*nu))}\
},\
{\
	{12.*E*Iy/(L*L*L), 0, 0, 0, -6.*E*Iy/(L*L), 0},\
	{0, 12.*E*Iz/(L*L*L), 0, 6*E*Iz/(L*L), 0, 0},\
	{0, 0, A*E/L, 0, 0, 0},\
	{0, 6*E*Iz/(L*L), 0 , 4*E*Iz/L, 0, 0},\
	{-6*E*Iy/(L*L), 0, 0, 0, 4*E*Iy/L, 0},\
	{0, 0, 0, 0, 0, E*J/(2*L*(1*nu))}\
},\
{\
	{A*E/L, 0, 0, 0, 0, 0},\
	{0, 12*E*Iz/(L*L*L),  0, 0, 0, -6*E*Iz/(L*L)},\
	{0, 0, 12*E*Iy/(L*L*L), 0, 6*E*Iy/(L*L), 0},\
	{0, 0, 0, E*J/(2*L*(1+nu)), 0, 0},\
	{0, 0, 6*E*Iy/(L*L), 0, 4*E*Iy/L, 0},\
	{0, -6*E*Iz/(L*L), 0, 0, 0, 4*E*Iz/L}\
}\
}[i][j][k])


#define K112(i,j,k)((const double[3][6][6]) {\
{\
	{-12.*E*Iy/(L*L*L), 0, 0, 0, 6*E*Iy/(L*L),0},\
	{0,-12.*E*Iz/(L*L*L), 0, -6*E*Iz/(L*L), 0, 0},\
	{0, 0, -A*E/L, 0, 0, 0},\
	{0, 6*E*Iz/(L*L), 0, 2*E*Iz/L, 0, 0},\
	{-6*E*Iy/(L*L), 0 ,0, 0, 2*E*Iy/L, 0},\
	{0, 0, 0, 0, 0, -E*J/(2*L*(1+nu))}\
},\
{\
	{-12.*E*Iy/(L*L*L), 0, 0, 0, -6*E*Iy/(L*L),0},\
	{0,-12.*E*Iz/(L*L*L), 0, 6*E*Iz/(L*L), 0, 0},\
	{0, 0, -A*E/L, 0, 0, 0},\
	{0, -6*E*Iz/(L*L), 0, 2*E*Iz/L, 0, 0},\
	{6*E*Iy/(L*L), 0 ,0, 0, 2*E*Iy/L, 0},\
	{0, 0, 0, 0, 0, -E*J/(2*L*(1+nu))}\
},\
{\
	{-A*E/L, 0, 0, 0, 0, 0},\
	{0, -12*E*Iz/(L*L*L),  0, 0, 0, -6*E*Iz/(L*L)},\
	{0, 0, -12*E*Iy/(L*L*L), 0, 6*E*Iy/(L*L), 0},\
	{0, 0, 0, -E*J/(2*L*(1+nu)), 0, 0},\
	{0, 0, -6*E*Iy/(L*L), 0, 2*E*Iy/L, 0},\
	{0, 6*E*Iz/(L*L), 0, 0, 0, 2*E*Iz/L}\
}\
	}[i][j][k])

using namespace BlinkyBlocks;

//enum PathState {NONE, BFS, ConfPath, Streamline};

class ForcesPredictionIPPTCode : public BlinkyBlocksBlockCode {


private:
	BlinkyBlocksBlock *module;

	double E=10; // elastic modulus
 	double L=4; //length
	double A=1; //cross sectional area
	double I=1/12.; //area
	double Iz = 1; //second moment of area
	double Iy = 1; //second moment of area
	double nu = 1; //Poisson ratio
	double J=1; //torsion constant

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
	ForcesPredictionIPPTCode(BlinkyBlocksBlock *host):BlinkyBlocksBlockCode(host) { module=host; };
	~ForcesPredictionIPPTCode() {};

	void startup();
	
	void SetNeighbors();
	void CheckNeighbors();

	void calculateU();

	bool isFixed(BlinkyBlocksBlock *modR);

	void printMatrix(vector< vector<double> > &matrix, int row=3, int col=3);
	void printVector(vector<double> &vec, int row=3);
	void clearNeighborsMessage(); //function to clear messages when calculated u

	void createK11(vector<bMatrix> &matrix);
	void createK12(vector<bMatrix> &matrix);

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
