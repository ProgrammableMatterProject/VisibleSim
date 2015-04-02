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
	Catoms3DGlBlock *ptrGlBlock; //!< ptr to the GL object
	Color color; //!< color of the block
	Cell3DPosition position; //!< position of the block in the grid of cells;
	short orientationCode; //!< number of the connector that is along the x axis.
    Catoms3DBlockCode *(*buildNewBlockCode)(Catoms3DBlock*);
/**
    \brief Constructor
    \param bId: id of the block
    \param catoms3DBlockCodeBuildingFunction : code block function
*/
	Catoms3DBlock(int bId, Catoms3DBlockCode *(*catoms3DBlockCodeBuildingFunction)(Catoms3DBlock*));
	~Catoms3DBlock();

	inline Catoms3DGlBlock* getGlBlock() { return ptrGlBlock; };
	inline void setGlBlock(Catoms3DGlBlock*ptr) { ptrGlBlock=ptr;};
	void setColor(const Color &);
	void setPositionAndOrientation(const Cell3DPosition &p,short code);
/**
    \brief Get the interface from the neighbor position in the grid
    \param pos: position of the cell (if in the grid)
    \return return interface if it exists one connected, NULL otherwise
*/
	P2PNetworkInterface *getInterface(const Cell3DPosition &pos);
	inline P2PNetworkInterface *getInterface(int d) { return tabInterfaces[d]; };
/**
    \brief Get the position of the gridcell in the direction of the given connector
    \param connectorId: id of connector (0..11)
    \param pos: position of the cell (if in the grid)
    \return return true if the cell is in the grid, false otherwise.
*/
	bool getNeighborPos(short connectorId,Cell3DPosition &pos);

	int getDirection(P2PNetworkInterface*);

	static short getOrientationFromMatrix(const Matrice &mat);
};

std::ostream& operator<<(std::ostream &stream, Catoms3DBlock const& bb);

}

#endif /* CATOMS3DBLOCK_H_ */
