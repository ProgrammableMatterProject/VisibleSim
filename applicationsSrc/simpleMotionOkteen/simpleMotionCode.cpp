#include "simpleMotionCode.h"
#include "okteenEvents.h"

void SimpleMotionCode::startup() {
	console << "start " << module->blockId << "\n";
	if (module->blockId==1) { // master id is 1
		//Vector3D finalPosition;
		OkteenMotions motion(module,SCLattice::Left,SCLattice::Top);
        step=0;
		scheduler->schedule(new OkteenMotionsStartEvent(scheduler->now()+1000,motion));
	}
}

void SimpleMotionCode::onMotionEnd() {
    if (step==0) {
        module->setColor(RED);
        OkteenMotions motion(module,SCLattice::Left,SCLattice::Top);
        scheduler->schedule(new OkteenMotionsStartEvent(scheduler->now()+1000,motion));
        step=1;
    } else {
        module->setColor(BLUE);
    }
}
