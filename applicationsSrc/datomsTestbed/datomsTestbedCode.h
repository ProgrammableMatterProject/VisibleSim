#ifndef datomsTestbedCode_H_
#define datomsTestbedCode_H_
#include "datomsSimulator.h"
#include "datomsBlockCode.h"
#include "datomsMotionRules.h"

using namespace Datoms;

class DatomsTestbedCode : public DatomsBlockCode {
private:
	DatomsBlock *module;
	SkewFCCLattice *lattice;
	
public :
	DatomsTestbedCode(DatomsBlock *host):DatomsBlockCode(host) { module = host; };
	~DatomsTestbedCode() {};

	void startup();

	void onMotionEnd();
/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
		return(new DatomsTestbedCode((DatomsBlock*)host));
	};
/*****************************************************************************/
};

#endif /* datomsTestbedCode_H_ */
