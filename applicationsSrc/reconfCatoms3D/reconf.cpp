#include "reconf.h"

Reconf::Reconf(Catoms3D::Catoms3DBlock *c) : catom(c)
{
    numberSeedsLeft = 0;
    numberSeedsRight = 0;
    lineCompleted = false;
    lineParent = false;
    seedNext = false;
    seedPrevious = false;
    leftCompleted = false;
    rightCompleted = false;
}

bool Reconf::isInternalSeed(LINE_DIRECTION lineDirection)
{
    if (!BlockCode::target->isInTarget(catom->position.addX(lineDirection).addY(lineDirection)) &&
            BlockCode::target->isInTarget(catom->position.addY(lineDirection)) ){
        return true;
    }
    return false;
}

bool Reconf::isBorderSeed(LINE_DIRECTION lineDirection)
{
    if (!BlockCode::target->isInTarget(catom->position.addX(lineDirection)) &&
        BlockCode::target->isInTarget(catom->position.addY(lineDirection)) ){
        return true;
    }
    return false;
}

// A sync module cant be seed to avoid two seeds constructing the same line (merge lines and avoid cycle)
bool Reconf::isSeedNext()
{
    return seedNext = seedNext || ((isInternalSeed(LINE_DIRECTION::TO_NEXT) || isBorderSeed(LINE_DIRECTION::TO_NEXT)) && !needSyncToRight());
}

bool Reconf::isSeedPrevious()
{
    return seedPrevious = seedPrevious || (isInternalSeed(LINE_DIRECTION::TO_PREVIOUS) || isBorderSeed(LINE_DIRECTION::TO_PREVIOUS)); //&& !needSyncToRightPrevious();
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

void Reconf::setLeftCompleted()
{
    leftCompleted = true;
    if (leftCompleted && rightCompleted)
        setLineCompleted();
}

void Reconf::setRightCompleted()
{
    rightCompleted = true;
    if (leftCompleted && rightCompleted)
        setLineCompleted();
}
