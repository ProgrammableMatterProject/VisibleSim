/*!
 * \file catoms3DBlock.h
 * \brief catoms Block
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#ifndef CATOMS3DBLOCK_H_
#define CATOMS3DBLOCK_H_

#include "buildingBlock.h"
#include "catoms3DBlockCode.h"
#include "catoms3DGlBlock.h"
#include "cell3DPosition.h"
//#include "catoms3DCapabilities.h"
#include <boost/asio.hpp>
#include <stdexcept>

//! \namespace Catoms3D
namespace Catoms3D {

class Catoms3DBlockCode;

class Catoms3DBlock : public BaseSimulator::BuildingBlock {
	P2PNetworkInterface *tabInterfaces[12];
protected:
	boost::interprocess::interprocess_mutex mutex_vm;

public:
	Catoms3DGlBlock *ptrGlBlock;
	Color color; //!< color of the block
	Cell3DPosition position; //!< position of the block in the grid of cells;
	short orientationCode; //! number of the connector that is along the x axis.
    Catoms3DBlockCode *(*buildNewBlockCode)(Catoms3DBlock*);
/**
    \brief Constructor
    \param bId : id of the block
    \param catoms3DBlockCodeBuildingFunction : code block function
*/
	Catoms3DBlock(int bId, Catoms3DBlockCode *(*catoms3DBlockCodeBuildingFunction)(Catoms3DBlock*));
	~Catoms3DBlock();

	inline Catoms3DGlBlock* getGlBlock() { return ptrGlBlock; };
	inline void setGlBlock(Catoms3DGlBlock*ptr) { ptrGlBlock=ptr;};
	void setColor(const Color &);
	void setPositionAndOrientation(const Cell3DPosition&p,short code);
	P2PNetworkInterface *getP2PNetworkInterfaceByRelPos(const PointRel3D &pos);
	inline P2PNetworkInterface *getInterface(int d) { return tabInterfaces[d]; }

	int getDirection(P2PNetworkInterface*);

	static short getOrientationFromMatrix(const Matrice &mat);
};

std::ostream& operator<<(std::ostream &stream, Catoms3DBlock const& bb);

}

#endif /* CATOMS3DBLOCK_H_ */
