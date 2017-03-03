#include "reconf.h"
#include "CSG/csgUtils.h"

Reconf::Reconf(Catoms3D::Catoms3DBlock *c) : catom(c)
{
    numberSeedsLeft = numberSeedsRight = 0;
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
    if (numberSeedsLeft == 0 &&
        !CsgUtils::isInside(catom->position.addX(1)) && 
        CsgUtils::isInside(catom->position.addY(1)) ){
        return true;
    }
    return false;
}

bool Reconf::isSeed()
{
    if (catom->blockId == 148)
        return false;
    return isInternalSeed() || isBorderSeed();
}

bool Reconf::needSyncToRight()
{
    if (!CsgUtils::isInside(catom->position.addX(1)) && 
        CsgUtils::isInside(catom->position.addX(1).addY(1)))
    { 
        for (int i = 2; catom->position.addX(i)[0] < CsgUtils::boundingBox.P1[0]; i++) {
            if (!CsgUtils::isInside(catom->position.addX(i)) && 
                CsgUtils::isInside(catom->position.addX(i).addY(1)))
                continue;
            if (CsgUtils::isInside(catom->position.addX(i)) && 
                CsgUtils::isInside(catom->position.addX(i).addY(1)) )
                return true;
        }
    }
    return false;
}

bool Reconf::needSyncToLeft()
{
    if (!CsgUtils::isInside(catom->position.addY(-1)) &&
        CsgUtils::isInside(catom->position.addX(-1)) &&
        CsgUtils::isInside(catom->position.addX(-1).addY(-1)) )
        return true;
    return false;
}

bool Reconf::needSync()
{
    return needSyncToLeft() || needSyncToRight();
}
