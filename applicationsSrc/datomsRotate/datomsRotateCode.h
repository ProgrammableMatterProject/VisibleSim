#ifndef datomsRotateCode_H_
#define datomsRotateCode_H_
#include <queue>
#include "datomsSimulator.h"
#include "datomsBlockCode.h"
#include "datomsMotionRules.h"
#include "teleportationEvents.h"

using namespace Datoms;

class DatomsRotateCode : public DatomsBlockCode {
private:
	DatomsBlock *module;
	SkewFCCLattice *lattice;
	queue <Cell3DPosition> cellsList;
public :
	DatomsRotateCode(DatomsBlock *host):DatomsBlockCode(host) { module = host; };
	~DatomsRotateCode() {};

	void startup();
	void initDistances();
	bool tryToMove();

	void onMotionEnd();
/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
		return(new DatomsRotateCode((DatomsBlock*)host));
	};
/*****************************************************************************/
};

#endif /* datomsRotateCode_H_ */
