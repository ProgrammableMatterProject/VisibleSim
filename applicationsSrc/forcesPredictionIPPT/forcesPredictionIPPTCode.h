#ifndef forcesPredictionIPPTCode_H_
#define forcesPredictionIPPTCode_H_
#include "robots/blinkyBlocks/blinkyBlocksSimulator.h"
#include "robots/blinkyBlocks/blinkyBlocksBlockCode.h"
#include "kMatrixes.h"

/* Message types (size of data vector in parenthesis) */

static const int TREE_MSG =1001; // build spanning tree messages (0)
static const int TREE_CONF_MSG =1002; // confirm/reject the edge of tree (1 -- True/False)

static const int CM_Q_MSG =1101; // center-of-mass query (0)
static const int CM_R_MSG =1102; // center-of-mass return (4 -- aggregated position times mass (3), aggregated mass(1) )

static const int DU_INIT_MSG =1201; // initiate weighted-Jacobi iteration procedure (1 -- how many iterations to make )
static const int DU_MSG =1202; // du exchange messages for weighted-Jacobi iterations (6 -- 6-DOF of the current solution )
static const int DU_COMPLETE_MSG =1203; // information about completion of weighted-Jacobi iterations in the structure (0)

static const int SST_Q_MSG =1301; // simplified stability check query (2 -- X-Y co-ordinates of the center of mass)
static const int SST_R_MSG =1302; // simplified stability check return (2 -- safe angle range, if 1st component ==-100 => zero angle; if 1st comopnent == 100 => full angle )

static const int MST_Q_MSG =1401; // model-based stability check query (0)
static const int MST_R_MSG =1402; // model-based stability check return (3+3+1 -- 3D co-ordinates of up to two points + number of points (0, 1, 2, 3, where 3 means stable support) )

static const int CG_INIT_Q_MSG = 1500; // initiate Conjugate Gradients iterative procedure (0)
static const int CG_INIT_R_MSG = 1501; // aggregate r.r after initialization of CG (1, r.r)
static const int CG_ALPHA_Q_MSG = 1502; // initialize first step of Conjugate Gradients iteration (1, beta)
static const int CG_ALPHA_R_MSG = 1503; // aggregate first step of Conjugate Gradients iteration (1, d.K.d) 
static const int CG_BETA_Q_MSG = 1504; // initialize second step of Conjugate Gradients iteration (1, alpha)
static const int CG_BETA_R_MSG = 1505; // aggregate second step of Conjugate Gradients iteration (1, r.r)
static const int CG_D_MSG = 1506; // send d vector in Conjugate Gradients iteration (6, d, to all neighbours)
static const int CG_DU_MSG = 1507; // send du vector in Conjugate Gradients iteration (6, du, to all neighbours)
static const int CG_VIS_Q_MSG = 1508; // initiate visualization & breakage check of CG method (0) 

// solvers & preconditioners

static const int SolverWeightedJacobi = 1;
static const int SolverConjugateGradients = 2;

static const int PreconditionerNone = 1;
static const int PreconditionerJacobi = 2;

// definitions of useful macros

#define TO_CHILDREN_TAIL(...) \
                else if(neighbors[i][1]==2) { /* if virtual neighbour */  \
                    __VA_ARGS__ \
                } \
            }

#define TO_CHILDREN(...) \
            for(int i=0; i<6; i++) {\
                if(tree_child[i]) { /* if non-virtual child */ \
                    P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(neighbors[i][0]); \
                    if(p2p) { \
                        __VA_ARGS__ \
                    } \
                } \
            TO_CHILDREN_TAIL

#define NORM2(v) ((v)[0]*(v)[0]+(v)[1]*(v)[1]+(v)[2]*(v)[2])

#define TO_PARENT(...) \
            P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(tree_par); \
            if(p2p) __VA_ARGS__

//////////

#define vectorSize 6

#define sign(v) ((v)<0?-1:((v)>0))

using namespace BlinkyBlocks;


class cmData {
    public:
        double mX=0, mY=0; // we assume that the floor is at Z=0 and that the gravity acts along Z direction
        double m=0;
        cmData(double mx, double my, double mass):mX(mx),mY(my),m(mass){};
        cmData() {};
};

class sstData {
    public:
        double X, Y; // we assume that the floor is at Z=0 and that the gravity acts along Z direction
        sstData(double x, double y):X(x),Y(y){};
        sstData():X(-100),Y(0){};
};

class mstData {
    public:
        vector<double> X[2]={{0.,0.,0.},{0.,0.,0.}}; // 3D co-ordinates of up to two points
        int nOfPts=0; // number of non-colinear points: 0 - no points are returned; 1-2 - unstable configuration; 3 - stable configuration
        mstData(vector<double> x, vector<double> y,int nop):nOfPts(nop){X[0]=x; X[1]=y;};
        mstData() {};
};

//enum PathState {NONE, BFS, ConfPath, Streamline};

class ForcesPredictionIPPTCode : public BlinkyBlocksBlockCode {

private:
    BlinkyBlocksBlock *module;

    double E; // elastic modulus
    double L; //length of the arm
    double a; // width of the cross-section of the square arm
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
//	double supportZ; //Z coordinate of the bottom modules (contacting with the support)

    typedef vector< vector<double> > bMatrix;

    bool isSupport = false;
    bool isFixed = false;
    bool isCentroid = false;
    bool performSST = false; // whether to perform the simplified stability check (only concerns the centroid module)
    int solver = SolverWeightedJacobi; // which type of solver to use
    int preconditioner = PreconditionerNone; // which type of solver to use
    bMatrix precK = decltype(precK)(6, vector<double>(vectorSize,0)); // precondioning matrix

    vector<double> orient={0,0,-1,0,0,0};

    int curIteration = 0; // current iteration
    int maxIterations = 0; // max number of iterations


//	matrix for visualization pf calculated forces and moments
//	bMatrix vizTable = decltype(vizTable)(6, vector<double>(vectorSize,0));
    bMatrix vizTable = decltype(vizTable)(6, vector<double>(vectorSize,0));

    //vector<bMatrix> K11 = decltype(K11)(6,vector<vector<double> >(3,vector <double>(3,0))); //K11 vector
    //vector<bMatrix> K12 = decltype(K12)(6,vector<vector<double> >(3,vector <double>(3,0))); //K12 vector

    vector <double> dup = decltype(dup)(vectorSize,0); // vector u from the previus iteration
    bMatrix uq = decltype(uq)(6, vector<double>(vectorSize,0)); //vectors u from 1-step neighbors
    vector <double> CGd = decltype(CGd)(vectorSize,0); // vector d for CG method
    bMatrix CGdq = decltype(CGdq)(6, vector<double>(vectorSize,0)); //vectors d for CG method from 1-step virtual neighbors
    vector <double> CGr = decltype(CGr)(vectorSize,0); // vector r for CG method
    bMatrix CGrq = decltype(CGrq)(6, vector<double>(vectorSize,0)); //vectors r for CG method from 1-step virtual neighbors
    vector <double> CGKd = decltype(CGKd)(vectorSize,0); // vector K.d for CG method
    bMatrix CGKdq = decltype(CGKdq)(6, vector<double>(vectorSize,0)); //vectors K.d for CG method from 1-step virtual neighbors
    double CGrr=0; // r.r for CG method
    double CGFpFp=1; // Fp.Fp for CG method 
    double CGdKd=0; // d.K.d for CG method
    double CGrnrn=0; // rn.rn for CG method


    vector <double> fp = decltype(fp)(vectorSize,0);
    vector <double> Fp = decltype(Fp)(vectorSize,0);

    bID neighbors[6][2];// Neighbors 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1 ///// second row: 1 if there is a message with du/d; 2 if the module is virtual (to be attached)
    bID tree_par=0; // spanning tree - parent's ID
    int tree_child[6]; // spanning tree: 0 - is not a child, 1 - is a child
    bool aggregationCompleted[6]; // checks if data aggregation is completed for all children (must be true for all children)
    cmData cmd;
    mstData mstd;
    sstData sstd;
public :
    vector <double> du = decltype(du)(vectorSize,0); // vector u from the current iteration


    ForcesPredictionIPPTCode(BlinkyBlocksBlock *host):BlinkyBlocksBlockCode(host) { module=host; };
    ~ForcesPredictionIPPTCode() {};

    void startup() override;

    void SetNeighbors();

    vector< vector<double> > contactStiffnessMatrix(vector<double> &dup); // stiffness matrix for contact with the floor
    void modelBasedStability(mstData &m, int who); // updates the info about the tilting condition per module. who==-1 is self assesment, who>=0 is assesment for a virtual neighbour 'who'.
    void aggregateModelBasedStability(mstData &mstd,mstData &mstdn); // aggregate model based stability data
    void simplifiedStability(sstData &m, vector<double> &cm, int who); // updates the angle for simplified stability. who==-1 is self assesment, who>=0 is assesment for a virtual neighbour 'who'.
    void aggregateSimplifiedStability(sstData &sstd,sstData &sstdn); // aggregate simplified stability data
    void computeDU(bool isInit=false);
    void computeNeighborDU(int i);
    void visualization();

//	bool isFixed(BlinkyBlocksBlock *modR);

//	bool isSupport(BlinkyBlocksBlock *modR);

    void printNeighbors();
    void printMatrix(vector< vector<double> > matrix, int row=vectorSize, int col=vectorSize, string desc="");
    void printVector(vector<double> vec, int row=3,string desc="");
    void clearNeighborsMessage(); //function to clear messages when calculated u

    vector< vector<double> > createK11(int i);
    vector< vector<double> > createK12(int i);
    vector< vector<double> > createTfr(double Txx, double Txy, double Txz,double Tyx, double Tyy, double Tyz);
    vector< vector<double> > createTmx(double Txz);
    vector< vector<double> > createTmy(double Txz);
    vector< vector<double> > createTmz(double Txz);
    vector< vector<double> > createRot(int i);
    vector< vector<double> > IdentityMatrix6();
    vector< vector<double> > createR(vector< vector<double> > &A);
    vector< vector<double> > createD(vector< vector<double> > &A);
    vector< vector<double> > RevD(vector< vector<double> > &A);

    void treeMessage(P2PNetworkInterface *sender);
    void treeConfMessage(const MessageOf<int >*msg,P2PNetworkInterface *sender);
    void cmQMessage(P2PNetworkInterface *sender);
    void cmRMessage(const MessageOf<cmData>*msg,P2PNetworkInterface *sender);
    void duInitMessage(const MessageOf<int >*msg,P2PNetworkInterface *sender);
    void duMessage(const MessageOf<vector<double> >*msg,P2PNetworkInterface *sender);
    void duCompleteMessage(P2PNetworkInterface *sender);
    void sstQMessage(const MessageOf<vector<double> >*msg,P2PNetworkInterface *sender);
    void sstRMessage(const MessageOf<sstData>*msg,P2PNetworkInterface *sender);
    void mstQMessage(P2PNetworkInterface *sender);
    void mstRMessage(const MessageOf<mstData>*msg,P2PNetworkInterface *sender);

    void CGInitQMessage(P2PNetworkInterface *sender);
    void CGInitRMessage(const MessageOf<double >*msg,P2PNetworkInterface *sender);
    void CGAlphaQMessage(const MessageOf<double >*msg,P2PNetworkInterface *sender);
    void CGAlphaRMessage(const MessageOf<double >*msg,P2PNetworkInterface *sender);
    void CGBetaQMessage(const MessageOf<double >*msg,P2PNetworkInterface *sender);
    void CGBetaRMessage(const MessageOf<double >*msg,P2PNetworkInterface *sender);
    void CGDMessage(const MessageOf<vector<double> >*msg,P2PNetworkInterface *sender);
    void CGDUMessage(const MessageOf<vector<double> >*msg,P2PNetworkInterface *sender);
    void CGVisQMessage(P2PNetworkInterface *sender);

    void parseUserElements(TiXmlDocument* config) override;
    void parseUserBlockElements(TiXmlElement* config) override;
//    bool parseUserCommandLineArgument(int argc, char *argv[]) override;

    void onBlockSelected() override;
    void onAssertTriggered() override;

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return(new ForcesPredictionIPPTCode((BlinkyBlocksBlock*)host));
    };
/*****************************************************************************/
};
//OPERATORS



void _treeMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _treeConfMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _cmQMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _cmRMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _duInitMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _duMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _duCompleteMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _sstQMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _sstRMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _mstQMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _mstRMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _CGInitQMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _CGInitRMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _CGAlphaQMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _CGAlphaRMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _CGBetaQMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _CGBetaRMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _CGDMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _CGDUMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);
void _CGVisQMessage(BlockCode*,MessagePtr,P2PNetworkInterface *sender);

double operator*(const vector<double> vec1 ,const vector<double> vec2);
vector<double> operator*(const vector<double> vec ,const double  scal);
vector<double> operator*(const double  scal, const vector<double> vec);
vector<double> operator+(const vector<double> vec1 ,const vector<double> vec2);
vector<double> operator-(const vector<double> vec1, const vector<double> vec2);
vector<double> operator-(const vector<double> vec);
vector<double> operator*(const vector< vector<double> > A, const vector<double> vec);
vector< vector<double> > operator*(const vector< vector<double> > A,const double B);
vector< vector<double> > operator*(const double B, const vector< vector<double> > A);
vector< vector<double> > operator*(const vector< vector<double> > A,const vector< vector<double> > B);
vector< vector<double> > operator+(const vector< vector<double> > A,const vector< vector<double> > B);
vector< vector<double> > operator-(vector< vector<double> > A,vector< vector<double> > B);
vector< vector<double> > operator-(vector< vector<double> > A);

void vector2string(const std::vector<bID>&v,string &s);
#endif /* forcesPredictionIPPTCode_H_ */
