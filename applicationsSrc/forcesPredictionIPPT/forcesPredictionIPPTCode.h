#ifndef forcesPredictionIPPTCode_H_
#define forcesPredictionIPPTCode_H_
#include "blinkyBlocksSimulator.h"
#include "blinkyBlocksBlockCode.h"
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
static const int SST_R_MSG =1302; // simplified stability check return (2 -- safe angle range )

static const int MST_Q_MSG =1401; // model-based stability check query (0)
static const int MST_R_MSG =1402; // model-based stability check return (3+3 -- 3D co-ordinates of up to two points )


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
};

class mstData {
    public:
        vector<double> X, Y;
        mstData(vector<double> x, vector<double> y):X(x),Y(y){};
};

//enum PathState {NONE, BFS, ConfPath, Streamline};

class ForcesPredictionIPPTCode : public BlinkyBlocksBlockCode {

private:
    BlinkyBlocksBlock *module;

    double E; // elastic modulus
    double L; //length
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

    bool isSupport = false;
    bool isFixed = false;
    bool isCentroid = false;

    vector<double> orient={0,0,-1,0,0,0};

    int curIteration = 0; // current iteration
    int maxIterations = 0; // max number of iterations

    typedef vector< vector<double> > bMatrix;

//	matrix for visualization pf calculated forces and moments
//	bMatrix vizTable = decltype(vizTable)(6, vector<double>(vectorSize,0));
    bMatrix vizTable = decltype(vizTable)(6, vector<double>(vectorSize,0));

    //vector<bMatrix> K11 = decltype(K11)(6,vector<vector<double> >(3,vector <double>(3,0))); //K11 vector
    //vector<bMatrix> K12 = decltype(K12)(6,vector<vector<double> >(3,vector <double>(3,0))); //K12 vector

    vector <double> dup = decltype(dup)(vectorSize,0); // vector u from the previus iteration
    bMatrix uq = decltype(uq)(6, vector<double>(vectorSize,0)); //vector u from 1-step neighbors

    vector <double> du = decltype(du)(vectorSize,0); // vector u from the current iteration

    vector <double> fp = decltype(fp)(vectorSize,0);
    vector <double> Fp = decltype(Fp)(vectorSize,0);

    bID neighbors[6][2];// Neighbors 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1 ///// second row: 1 if tehre is a message with du; 2 if the module is virtual (to be attached)
    bID tree_par=0; // spanning tree - parent's ID
    int tree_child[6]; // spanning tree: 0 - is not a child, 1 - is a child
    bool aggregationCompleted[6]; // checks if data aggregation is completed for all children (must be true for all children)
    cmData cmd;
public :
    ForcesPredictionIPPTCode(BlinkyBlocksBlock *host):BlinkyBlocksBlockCode(host) { module=host; };
    ~ForcesPredictionIPPTCode() {};

    void startup() override;

    void SetNeighbors();

    vector< vector<double> > contactStiffnessMatrix(vector<double> &dup); // stiffness matrix for contact with the floor
    void computeDU();
    void computeNeighborDU(int i);
    void visualization();

//	bool isFixed(BlinkyBlocksBlock *modR);

//	bool isSupport(BlinkyBlocksBlock *modR);

    void printNeighbors();
    void printMatrix(vector< vector<double> > &matrix, int row=vectorSize, int col=vectorSize, string desc="");
    void printVector(vector<double> &vec, int row=3,string desc="");
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
    void sstQMessage(const MessageOf<sstData>*msg,P2PNetworkInterface *sender);
    void sstRMessage(const MessageOf<sstData>*msg,P2PNetworkInterface *sender);
    void mstQMessage(P2PNetworkInterface *sender);
    void mstRMessage(const MessageOf<mstData>*msg,P2PNetworkInterface *sender);

    void parseUserElements(TiXmlDocument* config) override;
    void parseUserBlockElements(TiXmlElement* config) override;
    bool parseUserCommandLineArgument(int argc, char *argv[]) override;

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
