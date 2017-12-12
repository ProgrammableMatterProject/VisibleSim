/* @file targetColoration.cpp
 * @author Florian Pescher
 * @date 07/12/2017
 * Color the modules in target.
 */
 
#include "catoms3DSimulator.h"
#include "catoms3DBlockCode.h"
#include "targetColorationBlockCode.h"

using namespace Catoms3D;

int main(int argc, char **argv) {	
	/* Start simulation by reading configuration file, 
	 * instantiating necessary components, and starting the scheduler.*/
	createSimulator(argc, argv, targetColorationBlockCode::buildNewBlockCode);

	/* createSimulator only returns at scheduler end.
     * Can perform some actions here before ending simulation... */

	deleteSimulator(); // Deletion of allocated memory
	
	return(0);
}
