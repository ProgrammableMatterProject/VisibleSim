#ifndef ROBOTBLOCKSMOTIONRULES_H
#define ROBOTBLOCKSMOTIONRULES_H

#include "lattice.h"
#include "robotBlocksBlock.h"
#include "rotation3DEvents.h"

//!< \namespace RobotBlocks
namespace RobotBlocks {
	enum NeighborDirection {SOUTH, EAST, TOP, BOTTOM, WEST, NORTH, NUM_CONDIRS};

class RobotBlocksMotionRule {
protected :	
	NeighborDirection pivot;
	bool isRot;
	vector<Cell3DPosition> mustBeFull;
	vector<Cell3DPosition> mustBeFree;
public :
	RobotBlocksMotionRule(NeighborDirection _pivot,bool _isRotation): pivot(_pivot), isRot(_isRotation) {};
	virtual ~RobotBlocksMotionRule();
	Cell3DPosition getFinalPosition(const RobotBlocksBlock *rb) const {
		return rb->position+mustBeFree[0];
	}
	virtual bool isValid(const RobotBlocksBlock *rb);
	inline bool isRotation() { return isRot; }
	inline short getFromID() { return (int)(pivot); }
	virtual short getToID()=0;
};
	
class RobotBlocksTranslationMotionRule:public RobotBlocksMotionRule {
public :
	NeighborDirection direction;
	
	RobotBlocksTranslationMotionRule(NeighborDirection _pivot,NeighborDirection _direction);
	short getToID() override { return (int)(direction);};
};

class RobotBlocksRotationMotionRule:public RobotBlocksMotionRule {
public :
	NeighborDirection axe;
	
	RobotBlocksRotationMotionRule(NeighborDirection _pivot,NeighborDirection _axe);
	short getToID() override { return (int)(axe);};
};

/**
    \class RobotBlocksMotionRules
    \brief Define the list of possible motions for a Robot block
**/
class RobotBlocksMotionRules {
public:
    RobotBlocksMotionRules();
    virtual ~RobotBlocksMotionRules();

      /**
       \brief Returns if c3d catom is able to turn from the orientation fromId to the toId one
       \param c3d: the catom
       \param fromId : initial connector
       \param toId : final connector
       \return true if c3d catom is able to turn from the orientation fromId to the toId one */
			bool isValid(const RobotBlocksBlock& c3d,NeighborDirection from, NeighborDirection to);
    /**
       \brief Get the list of valid motion rules from a connector for c3D catom
       \param rb: the robotBlock
       \return return a vector of pointer to valid motion rules */
		vector<RobotBlocksMotionRule*> getValidMotionList(const RobotBlocksBlock* rb);

protected:
private:
	vector<RobotBlocksMotionRule*> motionRules;
};

std::ostream& operator<<(std::ostream &stream, RobotBlocksMotionRule const& mr);

} // RobotBlocks namespace
#endif // ROBOTBLOCKSMOTIONRULES_H
