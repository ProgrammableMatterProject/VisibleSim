#include "reconf.h"
#include "CSG/csgUtils.h"

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
    if (!CsgUtils::isInside(catom->position.addX(1).addY(1)) &&
            CsgUtils::isInside(catom->position.addY(1)) ){
        return true;
    }
    return false;
}

bool Reconf::isBorderSeed()
{
    if (!CsgUtils::isInside(catom->position.addX(1)) && 
        CsgUtils::isInside(catom->position.addY(1)) ){
        return true;
    }
    return false;
}

bool Reconf::isSeed()
{
    return seed;
}

bool Reconf::isSeedCheck()
{
   return seed = isInternalSeed() || isBorderSeed();
}

bool Reconf::needSyncToRight()
{
    if (!CsgUtils::isInside(catom->position.addX(1)) && 
        CsgUtils::isInside(catom->position.addX(1).addY(1)))
    { 
        for (int i = 2; CsgUtils::getWorldPosition(catom->position.addX(i))[0] < CsgUtils::boundingBox.P1[0]; i++) {
            if (!CsgUtils::isInside(catom->position.addX(i)) && 
                CsgUtils::isInside(catom->position.addX(i).addY(1)))
                continue;
            if (CsgUtils::isInside(catom->position.addX(i)) && 
                CsgUtils::isInside(catom->position.addX(i).addY(1)) )
                return true;
            return false;
        }
    }
    return false;
}

bool Reconf::needSyncToLeft()
{
    if (catom->getInterface(catom->position.addX(-1))->connectedInterface == NULL &&
        !CsgUtils::isInside(catom->position.addY(-1)) &&
        CsgUtils::isInside(catom->position.addX(-1)) &&
        CsgUtils::isInside(catom->position.addX(-1).addY(-1)) )
        return true;
    return false;
}

bool Reconf::needSync()
{
    return needSyncToLeft() || needSyncToRight();
}
