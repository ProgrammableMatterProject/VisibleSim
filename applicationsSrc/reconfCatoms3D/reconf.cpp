#include "reconf.h"

Reconf::Reconf(Catoms3D::Catoms3DBlock *c) : catom(c)
{
    numberSeedsLeft = 0;
    numberSeedsRight = 0;
    lineCompleted = false;
    lineParent = false;
    seed = false;
}

bool Reconf::isInternalSeed()
{
    if (!BlockCode::target->isInTarget(catom->position.addX(1).addY(1)) &&
            BlockCode::target->isInTarget(catom->position.addY(1)) ){
        return true;
    }
    return false;
}

bool Reconf::isBorderSeed()
{
    if (!BlockCode::target->isInTarget(catom->position.addX(1)) && 
        BlockCode::target->isInTarget(catom->position.addY(1)) ){
        return true;
    }
    return false;
}

bool Reconf::isSeed()
{
    return seed;
}

// A sync module cant be seed to avoid two seeds constructing the same line
bool Reconf::isSeedCheck()
{
    return seed = (isInternalSeed() || isBorderSeed()) && !needSyncToRight();
}

bool Reconf::needSyncToRight()
{
    if (!BlockCode::target->isInTarget(catom->position.addX(1)) && 
        BlockCode::target->isInTarget(catom->position.addX(1).addY(1)))
    {
        BoundingBox bb;
        BlockCode::target->boundingBox(bb);
        for (int i = 2; static_cast<TargetCSG*>(BlockCode::target)->gridToWorldPosition(catom->position.addX(i))[0] < bb.P1[0]; i++) {
            if (!BlockCode::target->isInTarget(catom->position.addX(i)) && 
                BlockCode::target->isInTarget(catom->position.addX(i).addY(1)))
                continue;
            if (BlockCode::target->isInTarget(catom->position.addX(i)) && 
                BlockCode::target->isInTarget(catom->position.addX(i).addY(1)) )
                return true;
            return false;
        }
    }
    return false;
}

bool Reconf::needSyncToLeft()
{
    if (catom->getInterface(catom->position.addX(-1))->connectedInterface == NULL &&
        !BlockCode::target->isInTarget(catom->position.addY(-1)) &&
        BlockCode::target->isInTarget(catom->position.addX(-1)) &&
        BlockCode::target->isInTarget(catom->position.addX(-1).addY(-1)) )
        return true;
    return false;
}

bool Reconf::needSync()
{
    return needSyncToLeft() || needSyncToRight();
}
