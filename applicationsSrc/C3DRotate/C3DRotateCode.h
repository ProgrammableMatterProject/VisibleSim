#ifndef C3DRotateCode_H_
#define C3DRotateCode_H_
#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"

static const int LOCK_MSG=1001;
static const int ANSLOCK_MSG=1002;

using namespace Catoms3D;

class Motions {
public :
    vector <Cell3DPosition> tabCells;
    Catoms3DBlock *mobile,*fixed;
    Catoms3DMotionRulesLink *MRlist;

    Motions(Catoms3DBlock *m,Catoms3DBlock *f,Catoms3DMotionRulesLink *mrl);
    ~Motions();
};


class C3DRotateCode : public Catoms3DBlockCode {
private:
    Catoms3DBlock *module;
    bool isLocked;
    Motions *currentMotion;
public :
	C3DRotateCode(Catoms3DBlock *host):Catoms3DBlockCode(host) { module = host; };
	~C3DRotateCode() {};

	void startup();
	bool tryToMove();
	void myLockFunc(const MessageOf<Motions>*msg,P2PNetworkInterface *sender);
	void myAnsLockFunc(const MessageOf<bool>*msg,P2PNetworkInterface *sender);
    void onMotionEnd();
    void onTap(int);
/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
		return(new C3DRotateCode((Catoms3DBlock*)host));
	};
/*****************************************************************************/
};

void _myLockFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);
void _myAnsLockFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);

#endif /* C3DRotateCode_H_ */
