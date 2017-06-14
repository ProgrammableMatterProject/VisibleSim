#include "simpleMotionCode.h"
#include "translationEvents.h"

void SimpleMotionCode::startup() {
	console << "start " << module->blockId << "\n";
	if (module->blockId==1) { // master id is 1
		Vector3D finalPosition;
	    finalPosition.set(module->position.pt[0]+1,module->position.pt[1],0);
	    scheduler->schedule(new TranslationStartEvent(scheduler->now()+1000,module,finalPosition));
	}
}

void SimpleMotionCode::onMotionEnd() {
    module->setColor(RED);
}
