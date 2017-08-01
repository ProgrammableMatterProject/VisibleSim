#include "simpleMotionCode.h"
#include "okteenEvents.h"

void SimpleMotionCode::startup() {
	console << "start " << module->blockId << "\n";
	if (module->blockId==1) { // master id is 1
		Vector3D finalPosition;
		OkteenMotions motion(module,SCLattice::Left,SCLattice::Top);

		scheduler->schedule(new OkteenMotionsStartEvent(scheduler->now()+1000,motion));
	}
}

void SimpleMotionCode::onMotionEnd() {
    module->setColor(RED);
}
