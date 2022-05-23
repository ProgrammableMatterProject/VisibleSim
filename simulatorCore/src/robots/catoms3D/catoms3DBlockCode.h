/*
 * catoms3DBlockCode.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS3DBLOCKCODE_H_
#define CATOMS3DBLOCKCODE_H_

#include <ostream>

#include "../../base/blockCode.h"
#include "catoms3DBlock.h"
#include "../../comm/network.h"
#include "../../events/scheduler.h"

using namespace BaseSimulator;

namespace Catoms3D {

/* class Catoms3DMotionRules; */
class Catoms3DBlock;

class Catoms3DBlockCode : public BaseSimulator::BlockCode {
     /* Catoms3DMotionRules *motionRules; */
public:
    Catoms3DBlockCode(Catoms3DBlock *host);
    virtual ~Catoms3DBlockCode();

    virtual void processLocalEvent(EventPtr pev) override;

};
}

#endif /* CATOMS3DBLOCKCODE_H_ */
