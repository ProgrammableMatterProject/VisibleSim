#ifndef datomsRotateCode_H_
#define datomsRotateCode_H_
#include "datomsSimulator.h"
#include "datomsBlockCode.h"
#include "datomsMotionRules.h"

static const int LOCK_MSG=1001;
static const int ANSLOCK_MSG=1002;
static const int UNLOCK_MSG=1003;

using namespace Datoms;

class Motions {
public :
    vector <Cell3DPosition> tabCells;
    DatomsBlock *mobile,*fixed;
    DatomsMotionRulesLink *MRlist;

    Motions(DatomsBlock *m,DatomsBlock *f,DatomsMotionRulesLink *mrl);
    ~Motions();
};


class DatomsRotateCode : public DatomsBlockCode {
private:
    DatomsBlock *module;
    bool isLocked;
    Motions *currentMotion;
	FCCLattice2 *lattice;
public :
	DatomsRotateCode(DatomsBlock *host):DatomsBlockCode(host) { module = host; };
	~DatomsRotateCode() {};

	void initDistances();

	void startup();
	bool tryToMove();
	void myLockFunc(const MessageOf<Motions>*msg,P2PNetworkInterface *sender);
	void myAnsLockFunc(const MessageOf<bool>*msg,P2PNetworkInterface *sender);
    void myUnlockFunc(P2PNetworkInterface *sender);
    void onMotionEnd();
    void onTap(int);
	
	//void parseUserElements(TiXmlDocument *config);
/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
		return(new DatomsRotateCode((DatomsBlock*)host));
	};
/*****************************************************************************/
};

void _myLockFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);
void _myAnsLockFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);
void _myUnlockFunc(BlockCode *,MessagePtr,P2PNetworkInterface *sender);

#endif /* datomsRotateCode_H_ */
