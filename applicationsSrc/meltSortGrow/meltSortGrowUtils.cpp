
#include "meltSortGrowUtils.h"

bool
MeltSortGrowUtils::motionIsPossible(Lattice *lattice,
                                    const Cell3DPosition &pFrom,
                                    const Cell3DPosition &pTo,
                                    const Cell3DPosition &pPivot)
{
    // A translation motion is possible if both positions are on a common plane
    //  and adjacent
    if (pFrom.pt[0] == pTo.pt[0] && pFrom.pt[1] == pTo.pt[1]) {
        return std::abs(pTo.pt[2] - pFrom.pt[2]) == 1;
    } else if (pFrom.pt[0] == pTo.pt[0] && pFrom.pt[2] == pTo.pt[2]) {
        return std::abs(pTo.pt[1] - pFrom.pt[1]) == 1;
    } else if (pFrom.pt[1] == pTo.pt[1] && pFrom.pt[2] == pTo.pt[2]) {
        return std::abs(pTo.pt[0] - pFrom.pt[0]) == 1;
    } else {
        // A rotation motion might be possible
        // Determine direction of rotation and check that there is no blocking module
        // .Consider deformation for now
        Cell3DPosition posThatMustBeClear = pTo - pPivot + pFrom;
        return lattice->isFree(posThatMustBeClear);
    }
}
