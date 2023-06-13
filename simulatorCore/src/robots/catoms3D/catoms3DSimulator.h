/*
 * catoms3DSimulator.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoit
 */

#ifndef CATOMS3DSIMULATOR_H_
#define CATOMS3DSIMULATOR_H_

#include "../../base/simulator.h"
#include "catoms3DBlockCode.h"
#include "catoms3DWorld.h"
#include "../../utils/trace.h"

using namespace std;

namespace Catoms3D {

    class Catoms3DSimulator : public BaseSimulator::Simulator {
    protected:

        Catoms3DSimulator(int argc, char *argv[], BlockCodeBuilder bcb);

        virtual ~Catoms3DSimulator();

    public:
        /// CUSTOMIZATION PARAMETERS
        bool useSkewedFCCLattice; //!< Indicates whether an FCC lattice with a skewed Z axis should be used instead for a normal FCC lattice, for FCC simulations
        ////////////////////////////

        static void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb,
                                    bool useSkewedFCCLattice);

        static Catoms3DSimulator *getSimulator() {
            assert(simulator != NULL);
            return ((Catoms3DSimulator *) simulator);
        }

        virtual void loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                               int argc, char *argv[]) override;

        virtual void loadBlock(TiXmlElement *blockElt, bID blockId, BlockCodeBuilder bcb,
                               const Cell3DPosition &pos, const Color &color, uint8_t orient) override;

        virtual void printInfo() override { OUTPUT << "I'm a Catoms3DSimulator" << endl; }

        void help();
    };

    inline void createSimulator(int argc, char *argv[], BlockCodeBuilder bcb,
                                bool useSkewedFCCLattice = false) {
        Catoms3DSimulator::createSimulator(argc, argv, bcb, useSkewedFCCLattice);
    }

    inline Catoms3DSimulator *getSimulator() { return (Catoms3DSimulator::getSimulator()); }

} // Catoms3D namespace
#endif /* CATOMS3DSIMULATOR_H_ */
