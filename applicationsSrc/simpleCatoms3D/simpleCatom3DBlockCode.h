/*
 * simpleCatom3DBlockCode.h
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#ifndef SIMPLECATOM3DBLOCKCODE_H_
#define SIMPLECATOM3DBLOCKCODE_H_

#include "robots/catoms3D/catoms3DBlockCode.h"
#include "robots/catoms3D/catoms3DSimulator.h"

#include "robots/catoms3D/catoms3DRotationEvents.h"
#include "robots/catoms3D/catoms3DBlock.h"

class SimpleCatom3DBlockCode : public Catoms3D::Catoms3DBlockCode {
public:
    Scheduler *scheduler;
    Catoms3D::Catoms3DBlock *catom;

    int step;
    int currentOr;
    double potentiel = 1.0;

    SimpleCatom3DBlockCode(Catoms3D::Catoms3DBlock *host);
    ~SimpleCatom3DBlockCode();

    void startup() override;
    void processLocalEvent(EventPtr pev) override;
    /* virtual bool getAttribute(const string &att,ostringstream &sout); */

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
            return (new SimpleCatom3DBlockCode((Catoms3DBlock*)host));
    }
    void nextRotation();

};


#endif /* SIMPLECATOM3DBLOCKCODE_H_ */
