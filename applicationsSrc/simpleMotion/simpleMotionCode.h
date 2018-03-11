#ifndef simpleMotionCode_H_
#define simpleMotionCode_H_
#include "smartBlocksSimulator.h"
#include "smartBlocksBlockCode.h"


using namespace SmartBlocks;

class SimpleMotionCode : public SmartBlocksBlockCode {
private:
	SmartBlocksBlock *module;
public:
	SimpleMotionCode(SmartBlocksBlock *host):SmartBlocksBlockCode(host) { module=host; };
	~SimpleMotionCode() {};

	void startup();
	virtual void onMotionEnd(void);

/*****************************************************************************/
/** needed to associate code to module                                      **/
	static BlockCode *buildNewBlockCode(BuildingBlock *host) {
		return(new SimpleMotionCode((SmartBlocksBlock*)host));
	};
/*****************************************************************************/
};


#endif /* simpleMotionCode_H_ */
