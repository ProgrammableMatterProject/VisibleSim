#ifndef SLIDINGCUBESMOTIONRULES_H
#define SLIDINGCUBESMOTIONRULES_H

#include "../../grid/lattice.h"
#include "slidingCubesBlock.h"
//#include "robots/catoms3D/catoms3DRotationEvents.h"

//!< \namespace SlidingCubes
namespace SlidingCubes {
    enum NeighborDirection {
        PlusX = 0, PlusY, PlusZ, MinusX, MinusY, MinusZ, NUM_CONDIRS
    };
//enum NeighborDirection {SOUTH, EAST, TOP, BOTTOM, WEST, NORTH, NUM_CONDIRS};

    class SlidingCubesMotionRule {
    protected :
        NeighborDirection pivot;
        bool isRot;
        vector<Cell3DPosition> mustBeFull;
        vector<Cell3DPosition> mustBeFree;
    public :
        SlidingCubesMotionRule(NeighborDirection _pivot, bool _isRotation) : pivot(_pivot), isRot(_isRotation) {};

        virtual ~SlidingCubesMotionRule();

        virtual void
        getFinalPositionAndOrientation(const SlidingCubesBlock *sc, Cell3DPosition &position, short &orientation) = 0;

        virtual bool isValid(const SlidingCubesBlock *rb);

        inline bool isRotation() { return isRot; }

        inline short getFromID() { return (int) (pivot); }

        virtual short getToID() = 0;
    };

    class SlidingCubesTranslationMotionRule : public SlidingCubesMotionRule {
    public :
        NeighborDirection direction;

        SlidingCubesTranslationMotionRule(NeighborDirection _pivot, NeighborDirection _direction);

        short getToID() override { return (int) (direction); };

        void getFinalPositionAndOrientation(const SlidingCubesBlock *sc, Cell3DPosition &position,
                                            short &orientation) override {
            position = sc->position + mustBeFree[0];
            orientation = sc->orientationCode;
        }
    };

    class SlidingCubesRotationMotionRule : public SlidingCubesMotionRule {
    public :
        NeighborDirection axe;

        SlidingCubesRotationMotionRule(NeighborDirection _pivot, NeighborDirection _axe);

        short getToID() override { return (int) (axe); };

        void getFinalPositionAndOrientation(const SlidingCubesBlock *sc, Cell3DPosition &position,
                                            short &orientation) override {
            position = sc->position + mustBeFree[0];
            Matrix m;
            switch (axe) {
                case PlusX:
                    m.setRotationX(180.0);
                    break;
                case PlusY:
                    m.setRotationY(180.0);
                    break;
                case PlusZ:
                    m.setRotationZ(180.0);
                    break;
                case MinusX:
                    m.setRotationX(-180.0);
                    break;
                case MinusY:
                    m.setRotationY(-180.0);
                    break;
                case MinusZ:
                    m.setRotationZ(-180.0);
                    break;
            }
            orientation = SlidingCubesBlock::getOrientationFromMatrix(m * sc->getGlBlock()->mat);
        }
    };

/**
   \class SlidingCubesMotionRules
   \brief Define the list of possible motions for a Robot block
**/
    class SlidingCubesMotionRules {
    public:
        SlidingCubesMotionRules();

        virtual ~SlidingCubesMotionRules();

        /**
           \brief Returns if c3d catom is able to turn from the orientation fromId to the toId one
           \param c3d: the catom
           \param fromId : initial connector
           \param toId : final connector
           \return true if c3d catom is able to turn from the orientation fromId to the toId one */
        bool isValid(const SlidingCubesBlock &c3d, NeighborDirection from, NeighborDirection to);

        /**
           \brief Get the list of valid motion rules from a connector for c3D catom
           \param rb: the robotBlock
           \return return a vector of pointer to valid motion rules */
        vector<SlidingCubesMotionRule *> getValidMotionList(const SlidingCubesBlock *rb);

    protected:
    private:
        vector<SlidingCubesMotionRule *> motionRules;
    };

    std::ostream &operator<<(std::ostream &stream, SlidingCubesMotionRule const &mr);

} // SlidingCubes namespace
#endif // SLIDINGCUBESMOTIONRULES_H
