/*!
 * \file catoms3DBlock.h
 * \brief catoms Block
 * \date 05/03/2015
 * \author Benoît Piranda
 */

#ifndef CATOMS3DBLOCK_H_
#define CATOMS3DBLOCK_H_

#include <stdexcept>

#include "buildingBlock.h"
#include "catoms3DBlockCode.h"
#include "catoms3DGlBlock.h"
#include "cell3DPosition.h"
//#include "catoms3DCapabilities.h"
#include "utils.h"

using namespace BaseSimulator::utils;

//! \namespace Catoms3D
namespace Catoms3D {

const float tabOrientationAngles[12][3] = { {0,0,0}, {180.0f,0.0f,-90.0f}, {-90.0f,45.0f,-45.0f},
											{90.0f,45.0f,-135.0f}, {-90.0f,45.0f,135.0f}, {90.0f,45.0f,45.0f},
											{0,0,180.0f}, {180.0f,0,90.0f}, {90.0f,-45.0f,135.0f},
											{-90.0f,-45.0f,45.0f}, {90.0f,-45.0f,-45.0f}, {-90.0f,-45.0f,-135.0f} };

const float tabConnectorPositions[12][3] = { {1,0,0}, {0,1,0}, {0.5,0.5,M_SQRT2_2},
											 {-0.5,0.5,M_SQRT2_2},{-0.5,-0.5,M_SQRT2_2},{0.5,-0.5,M_SQRT2_2},
											 {-1,0,0}, {0,-1,0}, {-0.5,-0.5,-M_SQRT2_2},
											 {0.5,-0.5,-M_SQRT2_2},{0.5,0.5,-M_SQRT2_2},{-0.5,0.5,-M_SQRT2_2}};


class Catoms3DBlockCode;

class Catoms3DBlock : public BaseSimulator::BuildingBlock {
protected:

public:
	short orientationCode; //!< number of the connector that is along the x axis.
/**
   \brief Constructor
   \param bId: id of the block
   \param catoms3DBlockCodeBuildingFunction : code block function
*/
	Catoms3DBlock(int bId, BlockCodeBuilder bcb);
	~Catoms3DBlock();

	inline virtual Catoms3DGlBlock* getGlBlock() { return static_cast<Catoms3DGlBlock*>(ptrGlBlock); };
	inline void setGlBlock(Catoms3DGlBlock*ptr) { ptrGlBlock=ptr;};

	void setVisible(bool visible);
	void setPositionAndOrientation(const Cell3DPosition &p,short code);
/**
   \brief Get the interface from the neighbor position in the grid
   \param pos: position of the cell (if in the grid)
   \return return interface if it exists one connected, NULL otherwise */
	P2PNetworkInterface *getInterface(const Cell3DPosition &pos);
	inline P2PNetworkInterface *getInterface(int d) { return P2PNetworkInterfaces[d]; };
/**
   \brief Get the position of the gridcell in the direction of the given connector
   \param connectorId: id of connector (0..11)
   \param pos: position of the cell (if in the grid)
   \return return true if the cell is in the grid, false otherwise. */
	bool getNeighborPos(short connectorId,Cell3DPosition &pos);

	int getDirection(P2PNetworkInterface*);

	static short getOrientationFromMatrix(const Matrix &mat);

	// MeldInterpreter
	/**
	 * @copydoc BuildingBlock::addNeighbor
	 */
	virtual void addNeighbor(P2PNetworkInterface *ni, BuildingBlock* target);
	/**
	 * @copydoc BuildingBlock::removeNeighbor
	 */
	virtual void removeNeighbor(P2PNetworkInterface *ni);

};

std::ostream& operator<<(std::ostream &stream, Catoms3DBlock const& bb);

}

#endif /* CATOMS3DBLOCK_H_ */
