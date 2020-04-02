/* @file molding.cpp
 * @author Florian Pescher
 * @date 07/12/2017
 * Color the modules in target.
 */

#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DBlockCode.h"
#include "moldingBlockCode.h"

using namespace Catoms3D;

int main(int argc, char **argv) {
    /* Start simulation by reading configuration file,
     * instantiating necessary components, and starting the scheduler.*/
    createSimulator(argc, argv, moldingBlockCode::buildNewBlockCode,
                    // useSkewedFCCLattice
                    true);

    /* createSimulator only returns at scheduler end.
     * Can perform some actions here before ending simulation... */


    deleteSimulator(); // Deletion of allocated memory

    return(0);
}
