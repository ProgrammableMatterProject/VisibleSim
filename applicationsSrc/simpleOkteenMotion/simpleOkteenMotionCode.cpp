#include "simpleOkteenMotionCode.h"
//#include "robots/okteen/okteenEvents.h"
#include "motion/teleportationEvents.h"

void SimpleOkteenMotionCode::startup() {
    console << "start " << module->blockId << " pos=" << module->position << "\n";
    members=NULL;
    if (module->position.pt[0]%2==0 && module->position.pt[1]%2==0 && module->position.pt[2]%2==0 ) {
        module->setColor(BLUE);
        members = new OkteenBlock*[8];
        members[0] = module;
        members[1] = (OkteenBlock*)lattice->getBlock(module->position+Cell3DPosition(1,0,0));
        members[2] = (OkteenBlock*)lattice->getBlock(module->position+Cell3DPosition(0,0,1));
        members[3] = (OkteenBlock*)lattice->getBlock(module->position+Cell3DPosition(0,0,1));

    } else if (module->position.pt[0]%2 + module->position.pt[1]%2 + module->position.pt[2]%2 <=1 ) {
        module->setColor(WHITE);
    } else {

        module->setColor(ORANGE);
    }
    /*if (module->blockId==1) { // master id is 1
        Cell3DPosition finalPosition = module->position+Cell3DPosition(2,0,0);
        cout << module->position << "->" << finalPosition << endl;
        TeleportationStartEvent *ev = new TeleportationStartEvent(scheduler->now()+1000,module,finalPosition);
        scheduler->schedule(ev);

    }*/

}

void SimpleOkteenMotionCode::onMotionEnd() {
    /*if (step==0) {
        module->setColor(RED);
        //OkteenMotions motion(module,SCLattice::Back,SCLattice::Bottom);
        OkteenMotions motion(module,SCLattice::Bottom,SCLattice::Front);
        scheduler->schedule(new OkteenMotionsStartEvent(scheduler->now()+1000,motion));
        step=1;
    } else {
        module->setColor(BLUE);
    }*/
    console << "end " << module->position << "\n";
}
