
/*
 * meltSortGrowUtils.h
 *
 *  Created on: 28/11/17
 *      Author: pthalamy
 */

#ifndef MELTSORTGROWUTILS_H_
#define MELTSORTGROWUTILS_H_

#include "lattice.h"

using namespace BaseSimulator;

class MeltSortGrowUtils {
public:

/**
 * @brief Return a boolean indicating whether a given C3D motion is possible using Catoms3D motion primitives and local constraints
 * @param lattice a pointer to the lattice in which the movement occurs
 * @param pFrom start position of the motion under consideration
 * @param pTo target position of the motion under consideration
 * @param pPivot pivot Catom of the movement, mostly relevant when considering rotations
 * @return true if motion is possible, false if local constraints do not allow it */
static bool motionIsPossible(Lattice *lattice,
                                  const Cell3DPosition &pFrom,
                                  const Cell3DPosition &pTo,
                                  const Cell3DPosition &pPivot);     
};
#endif /* MELTSORTGROWUTILS_H_ */
