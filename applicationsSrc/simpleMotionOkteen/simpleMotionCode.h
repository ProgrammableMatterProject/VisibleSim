#ifndef simpleMotionCode_H_
#define simpleMotionCode_H_

#include "okteenSimulator.h"
#include "okteenBlockCode.h"

using namespace Okteen;

class SimpleMotionCode : public OkteenBlockCode {
private:
	OkteenBlock *module;
	int step;
	// metamodule data
	// leader
	OkteenBlock* members;
	// path to leader
	P2PNetworkInterface *pathToLeader;
public:
	SimpleMotionCode(OkteenBlock *host):OkteenBlockCode(host) { module=host; };
	~SimpleMotionCode() {};

	void startup();
	virtual void onMotionEnd(void);
	void sendMessageToMMLeader();

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
		return(new SimpleMotionCode((OkteenBlock*)host));
	};
/*****************************************************************************/
};


#endif /* simpleMotionCode_H_ */
