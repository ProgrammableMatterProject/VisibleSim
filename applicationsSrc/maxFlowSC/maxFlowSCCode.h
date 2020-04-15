#ifndef maxFlowSCCode_H_
#define maxFlowSCCode_H_
#include "robots/slidingCubes/slidingCubesSimulator.h"
#include "robots/slidingCubes/slidingCubesBlockCode.h"

static const int BFS_MSG=1001;
static const int CONFIRM_EDGE_MSG=1002;
static const int CUT_OFF_MSG=1003;
static const int AVAILABLE_MSG=1004;
static const int CONFIRM_PATH_MSG=1005;
static const int CONFIRM_STREAMLINE_MSG=1005;

using namespace SlidingCubes;

enum PathState {NONE, BFS, ConfPath, Streamline};

class MaxFlowSCCode : public SlidingCubesBlockCode {
private:
    SlidingCubesBlock *module;
    PathState mainPathState;	//! state of the main path: {NONE, BFS, ConfPath, Streamline}
    bID mainPathIn;				//! ID of parent meta-module on the main tree
    vector<bID>mainPathOut;     //! ID’s of child meta-modules on the main tree
    vector<bID>mainPathOld;     //! old ID’s of child meta-modules on the main tree
    PathState aug1PathState;	//! state of the augmenting path 1: {NONE, BFS, ConfPath}
    bID aug1PathIn;				//! ID of parent meta-module on the augmenting tree (type 1)
    vector<bID>aug1PathOut;		//! ID’s of child meta-modules on the augmenting tree (type 1)
    vector<bID>aug1PathOld;		//! old ID’s of child meta-modules on the augmenting tree (type 1)
    PathState aug2PathState;	//! state of the augmenting path 2: {NONE, BFS, ConfPath}
    bID aug2PathIn;				//! ID of parent meta-module on the augmenting tree (type 2)
    vector<bID>aug2PathOut;		//! ID’s of child meta-modules on the augmenting tree (type 2)
    vector<bID>aug2PathOld;		//! old ID’s of child meta-modules on the augmenting tree (type 2)
    //vector<bID>pathsOld;		//! list of path ID’s that has been processed in current step
    bool isSource;				//! Is meta-module a source.
    bool isSink;

public :
    MaxFlowSCCode(SlidingCubesBlock *host):SlidingCubesBlockCode(host) { module=host; };
    ~MaxFlowSCCode() {};

    void startup() override;
    void ProcBFS(std::shared_ptr<Message> msg, P2PNetworkInterface *sender);
    void ProcConfirmEdge(std::shared_ptr<Message> msg, P2PNetworkInterface *sender);
    void ProcCutOff(std::shared_ptr<Message> msg, P2PNetworkInterface *sender);
    void ProcAvailable(std::shared_ptr<Message> msg, P2PNetworkInterface *sender);
    void ProcConfirmPath(std::shared_ptr<Message> msg, P2PNetworkInterface *sender);
    void ProcConfirmStreamline(std::shared_ptr<Message> msg, P2PNetworkInterface *sender);

    void sendMessageToPath(const string &str, int msgType,vector<bID> &path,bID exception);
/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return(new MaxFlowSCCode((SlidingCubesBlock*)host));
    };
/*****************************************************************************/
};

template <typename T> void operator+=(std::vector<T> &v1, const std::vector<T> &v2);
template <typename T> bool isIn(const std::vector<T> &v,T value);
template <typename T> bool operator==(T value,const std::vector<T> &v);
template <typename T> bool operator!=(T value,const std::vector<T> &v);

void vector2string(const std::vector<bID>&v,string &s);
#endif /* maxFlowSCCode_H_ */
