#include "simpleMotionCode.h"
#include "okteenEvents.h"

void SimpleMotionCode::startup() {
	console << "start " << module->blockId << " pos=" << module->position << "\n";

	if (module->position.pt[0]%2 + module->position.pt[1]%2 + module->position.pt[2]%2 <=1 ) {
        //isStructure=true;
        module->setColor(WHITE);
	} else {
        module->setColor(ORANGE);
	}
	if (module->blockId==1) { // master id is 1
		//Vector3D finalPosition;
		OkteenMotions motion(module,SCLattice::Back,SCLattice::Bottom);
        step=0;
		scheduler->schedule(new OkteenMotionsStartEvent(scheduler->now()+1000,motion));
	}

}

void SimpleMotionCode::onMotionEnd() {
    if (step==0) {
        module->setColor(RED);
        //OkteenMotions motion(module,SCLattice::Back,SCLattice::Bottom);
        OkteenMotions motion(module,SCLattice::Bottom,SCLattice::Front);
        scheduler->schedule(new OkteenMotionsStartEvent(scheduler->now()+1000,motion));
        step=1;
    } else {
        module->setColor(BLUE);
    }
}
