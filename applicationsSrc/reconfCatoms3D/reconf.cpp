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
    createdFromPrevious = true;
}

bool Reconf::isInternalSeedNext()
{
    if (catom->getInterface(catom->position.addY(1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.addX(1).addY(1)) &&
            BlockCode::target->isInTarget(catom->position.addY(1)) ){
        return true;
    }
    return false;
}

bool Reconf::isInternalSeedPrevious()
{
    if (catom->getInterface(catom->position.addY(-1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.addX(-1).addY(-1)) &&
            BlockCode::target->isInTarget(catom->position.addY(-1)) ){
        return true;
    }
    return false;
}

bool Reconf::isBorderSeedNext()
{
    if (catom->getInterface(catom->position.addY(1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.addX(1)) &&
        BlockCode::target->isInTarget(catom->position.addY(1)) ){
        return true;
    }
    return false;
}

bool Reconf::isBorderSeedPrevious()
{
    if (catom->getInterface(catom->position.addY(-1))->isConnected())
        return false;

    if (!BlockCode::target->isInTarget(catom->position.addX(-1)) &&
        BlockCode::target->isInTarget(catom->position.addY(-1)) ){
        return true;
    }
    return false;
}

// A sync module cant be seed to avoid two seeds constructing the same line (merge lines and avoid cycle)
bool Reconf::isSeedNext()
{
    seedNext = seedNext || ((isInternalSeedNext() || isBorderSeedNext()) && !needSyncToRightNext());
    if (seedNext)
        catom->setColor(LIGHTBLUE);
    return seedNext;
}

bool Reconf::isSeedPrevious()
{
    seedPrevious = seedPrevious || ((isInternalSeedPrevious() || isBorderSeedPrevious()) && !needSyncToRightPrevious());
    if (seedPrevious)
        catom->setColor(YELLOW);
    return seedPrevious;
}


bool Reconf::needSyncToRightNext()
{
    if (createdFromPrevious &&
        !BlockCode::target->isInTarget(catom->position.addX(1)) &&
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

bool Reconf::needSyncToRightPrevious()
{
    if (!createdFromPrevious &&
        !BlockCode::target->isInTarget(catom->position.addX(-1)) &&
        BlockCode::target->isInTarget(catom->position.addX(-1).addY(-1)))
    {
        BoundingBox bb;
        BlockCode::target->boundingBox(bb);
        for (int i = 2; static_cast<TargetCSG*>(BlockCode::target)->gridToWorldPosition(catom->position.addX(i))[0] < bb.P1[0]; i++) {
            if (!BlockCode::target->isInTarget(catom->position.addX(-i)) &&
                BlockCode::target->isInTarget(catom->position.addX(-i).addY(-1)))
                continue;
            if (BlockCode::target->isInTarget(catom->position.addX(-i)) &&
                BlockCode::target->isInTarget(catom->position.addX(-i).addY(-1)) )
                return true;
            return false;
        }
    }
    return false;
}

bool Reconf::needSyncToLeftNext()
{
    if (createdFromPrevious &&
        catom->getInterface(catom->position.addX(-1))->connectedInterface == NULL &&
        !BlockCode::target->isInTarget(catom->position.addY(-1)) &&
        BlockCode::target->isInTarget(catom->position.addX(-1)) &&
        BlockCode::target->isInTarget(catom->position.addX(-1).addY(-1)) )
        return true;
    return false;
}

bool Reconf::needSyncToLeftPrevious()
{
    if (!createdFromPrevious &&
        catom->getInterface(catom->position.addX(1))->connectedInterface == NULL &&
        !BlockCode::target->isInTarget(catom->position.addY(1)) &&
        BlockCode::target->isInTarget(catom->position.addX(1)) &&
        BlockCode::target->isInTarget(catom->position.addX(1).addY(1)) )
        return true;
    return false;
}

bool Reconf::needSync()
{
    return needSyncToLeftPrevious() || needSyncToRightPrevious();
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
