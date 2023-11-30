/*! @file target.h
 * @brief Defines a target configuration for reconfiguration algorithms,
 * several ways of defining the configuration are provided to the user.
 *
 * e.g. Definition of a list of targets in the XML file:
 *
 *      <targetList>
 *        <target format="grid">
 *          <cell position="0,0,0"/>
 *          <cell position="1,2,3" color="128,128,128"/>
 *          <cell color="128,128,128" position="3,2,1"/>
 *        </target>
 *        <target format="grid">
 *          <cell position="0,0,0"/>
 *          <cell position="1,1,1"/>
 *          <cell position="1,3,2"/>
 *          <cell position="2,2,2"/>
 *          <cell position="3,3,3"/>
 *        </target>
 *        <target format="csg">
 *          <csg content="union() { cube([10,10,10]); cylinder(10,5,5); }"/>
 *        </target>
 *        </targetList>
 *
 * @author Pierre Thalamy
 * @date 21/07/2016
 */

#ifndef TARGET_H__
#define TARGET_H__

#include <map>
#include <iostream>
#include <fstream>
#include <list>
#include <vector>
#include <cfloat>

#define TIXML_USE_STL    1

#include "../math/cell3DPosition.h"
#include "../deps/TinyXML/tinyxml.h"
#include "../utils/color.h"
#include "../utils/exceptions.h"
#include "../csg/csg.h"
#include "../math/vector3D.h"

using namespace std;

namespace BaseSimulator {

//<! @brief Abstract Target. Provides the user with functions for checking a target position and color.
    class Target {
    public: // exceptions
        class UnknownTargetFormatException : public VisibleSimException {
        public:
            UnknownTargetFormatException(const string &format) {
                stringstream ss;
                ss << "Unknown target format found in configuration file: "
                   << format << endl;
                m_msg = ss.str();
            }
        };

        //<! @brief Exception thrown if an error as occured during parsing
        class TargetParsingException : public VisibleSimException {
        public:
            TargetParsingException() :
                    VisibleSimException(std::string("Invalid target description in configuration file\n")) {}
        };

//<! @brief Exception thrown if the user is attempting to check a position that is not part of the target
        class InvalidPositionException : public VisibleSimException {
        public:
            InvalidPositionException(const Cell3DPosition &pos) {
                stringstream ss;
                ss << "Position does not belong to the target: "
                   << pos << endl;
                m_msg = ss.str();
            }
        };

        //<! @brief Exception thrown if the user provides incorrect dimensions for the target
        class InvalidDimensionsException : public VisibleSimException {
        public:
            InvalidDimensionsException(const Cell3DPosition &dim) {
                stringstream ss;
                ss << "Target dimensions are invalid: "
                   << dim << endl;
                m_msg = ss.str();
            }
        };

    protected:
        /**
         * @brief prints target to an ouput string
         * @param where ostream on which to print the object
         */
        virtual void print(ostream &where) const {};

    public:
        static TiXmlNode *targetListNode; //!< pointer to the target list node from the XML configuration file
        static TiXmlNode *targetNode; //!< pointer to the current target node from the XML configuration file

        /**
         * @brief Parse next target from the configuration file's Target List, and return a pointer to the instantiated object
         * @return pointer to the parsed Target object, or NULL if there are no (more) targets in the configuration file
         */
        static Target *loadNextTarget();

        /**
         * @brief Target constructor. Where parsing occurs thanks to the targetNode parameter
         * @param targetNode XML Node containing target description from configuration file
         */
        Target(TiXmlNode *targetNode) {};

        virtual ~Target() {};

        /**
         * @brief Indicates if a position belongs to the target
         * @param pos position to consider
         * @return true if pos belongs to the target, false otherwise
         */
        virtual bool isInTarget(const Cell3DPosition &pos) const = 0;

        /**
         * @brief Returns the target color at position pos
         * @param pos position to condiser
         * @return target color at cell p
         */
        virtual const Color getTargetColor(const Cell3DPosition &pos) const = 0;

        /*
         * @brief Draw geometry of the target in the interfaces
         */
        virtual void glDraw();

        /**
         * Highlights the target with semi-transparent colored cells
         */
        virtual void highlight() const = 0;

        /**
         * Cancels out Target::highlight
         */
        virtual void unhighlight() const = 0;

        friend ostream &operator<<(ostream &out, const Target *t);
    };  // class Target

//<! @brief A target modeled as a container of unique positions and colors.c
    class TargetGrid : public Target {
    protected:
        // Only store target cells instead of the entire grid to save memory
        map<const Cell3DPosition, const Color> tCells; //!< the target cells as Cell/Color key-value pairs

    protected:
        /**
         * @brief Add a cell to the target cells container
         * @param pos position of the target cell
         * @param c color of the cell. If none provided, defaults to (0,0,0,0)
         */
        void addTargetCell(const Cell3DPosition &pos, const Color c = Color());

        //!< @copydoc Target::print
        virtual void print(ostream &where) const override;

    public:
        /**
         * @copydoc Target::Target
         * XML Description Format:
         * <target format="grid">
         *   <cell position="x,y,z" color="r,g,b"/>
         *   ...
         * </target>
         */
        TargetGrid(TiXmlNode *targetNode);

        virtual ~TargetGrid() {};

        //!< @copydoc Target::getTargetColor
        //!< a cell is in the target grid if and only if it is present in the target cells container
        virtual bool isInTarget(const Cell3DPosition &pos) const override;

        //!< @copydoc Target::getTargetColor
        //!< @throws InvalidPositionException is cell at position pos is not part of the target
        virtual const Color getTargetColor(const Cell3DPosition &pos) const override;

        virtual void highlight() const override;

        virtual void unhighlight() const override;

        friend ostream &operator<<(ostream &f, const TargetGrid &tg);
    };  // class TargetGrid

//<! @brief A target modeled as a container of unique positions using a coordinate system relative to some specific origin module, and colors
    class RelativeTargetGrid : public TargetGrid {
    protected:
        Cell3DPosition *origin = NULL;

        //<! @brief Exception thrown if an there is an attempt to use the RelativeTargetGrid without having set its origin beforehand
        struct MissingInitializationException : std::exception {
            const char *what() const noexcept override {
                return "Attempted to call isInTarget without having set the target's origin first\n";
            }
        };

        std::map<const Cell3DPosition, int> geodesicToOrigin;

        void computeGeodesics();

    public:
        std::list<Cell3DPosition> *targetCellsInConstructionOrder = NULL; //todo protected

        RelativeTargetGrid(TiXmlNode *targetNode) : TargetGrid(targetNode) {};

        virtual ~RelativeTargetGrid() {
            delete origin;
            delete targetCellsInConstructionOrder;
        };

        //!< @copydoc Target::iInTargetColor
        //!< a cell is in the target grid if and only if it is present in the target cells container
        //!< @warning Can only be used once origin has been set, and expects a relative position as input
        virtual bool isInTarget(const Cell3DPosition &pos) const override;

        bool reconfigurationIsComplete() const;

        void highlightByDistanceToRoot() const;

        /**
         * @brief Returns a list of all cells in target in ascending order (x, y, and then z)
         * @param tgCells a reference to the output list of all cells in the target in ascending order
         * @warning Can only be used once origin has been set
         * @throw MissingInitializationException if target origin has not been set
         */
        list<Cell3DPosition> *getTargetCellsInConstructionOrder();

        /**
         * @brief Sets the origin of the coordinate system used by the target
         * @warning Calling Target::isInTarget before setting the origin will result in an error
         */
        virtual void setOrigin(const Cell3DPosition &org);

        void removeTargetCell(const Cell3DPosition &tc);

        /**
         * @brief For configuration design only, takes an absolute target as input and make it relative to the cell at pos (min_z, min_y, min_x), and prints the output to stdout
        */
        void relatifyAndPrint();
    };  // class RelativeTargetGrid

//<! @brief A target modeled as an ensemble of shapes
    class TargetCSG : public Target {
    public:
        CSGNode *csgRoot;
        BoundingBox bb;
        Vector3D translate; // Can be used to to offset the origin of the CSG object by x,y,z
        bool offsetBoundingBox; // Offsets entire CSG target by (1,1,1)
    protected:
        //!< @copydoc Target::print
        virtual void print(ostream &where) const override {};
    public:
        TargetCSG(TiXmlNode *targetNode);

        virtual ~TargetCSG() {};

        /**
         * @brief Returns the target bounding box
         * @param bb boundingbox to be written
         */
        virtual void boundingBox(BoundingBox &bb);

        //!< @copydoc Target::isInTarget
        virtual bool isInTarget(const Cell3DPosition &pos) const override;

        //!< @copydoc Target::getTargetColor
        virtual const Color getTargetColor(const Cell3DPosition &pos) const override;

        /**
         * @brief Grid to unscaled world position within bounding box
         * @param pos position of the target cell
         */
        Vector3D gridToCSGPosition(const Cell3DPosition &pos) const;

        /**
         * @brief Unscaled world position for CSG within bounding box to grid position
         * @param pos position of the target cell
         */
        Cell3DPosition CSGToGridPosition(const Vector3D &pos) const;

        /**
         * @brief The object is in the border of the target
         * @param pos position of the target cell
         * @param radius radius of the border
         */
        bool isInTargetBorder(const Cell3DPosition &pos, double radius) const;

        /*
         * @brief Draw geometry of the target in the interfaces
         */
        virtual void glDraw() override;

        virtual void highlight() const override;

        virtual void unhighlight() const override;

    };  // class TargetCSG

} // namespace BaseSimulator

#endif  // TARGET_H__
