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

#define TIXML_USE_STL	1
#include "TinyXML/tinyxml.h"

#include "color.h"
#include "cell3DPosition.h"
#include "csg.h"

using namespace std;

namespace BaseSimulator {

//<! @brief Abstract Target. Provides the user with functions for checking a target position and color.
class Target {
protected:

    /**
     * @brief prints target to an ouput string
     * @param where ostream on which to print the object
     */
    virtual void print(ostream& where) const {};

public:
    //<! @brief Exception thrown if an error as occured during parsing
    struct TargetParsingException : std::exception {
        const char* what() const noexcept {
            return "Invalid target description in configuration file\n";
        }
    };
    //<! @brief Exception thrown if the user is attempting to check a position that is not part of the target
    struct InvalidPositionException : std::exception {
        const char* what() const noexcept {
            return "Position does not belong to the target\n";
        }
    };
    //<! @brief Exception thrown if the user provides incorrect dimensions for the target
    struct InvalidDimensionsException : std::exception {
        const char* what() const noexcept {
            return "Target dimensions are invalid\n";
        }
    };

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
    virtual const Color getTargetColor(const Cell3DPosition &pos) = 0;

    /**
     * @brief Returns the target bounding box
     * @param bb boundingbox to be written
     */
    virtual void boundingBox(BoundingBox &bb) = 0;

    friend ostream& operator<<(ostream& out,const Target *t);
};  // class Target

//<! @brief A target modeled as a container of unique positions and colors.c
class TargetGrid : public Target {
protected:
     // Only store target cells instead of the entire grid to save memory
    map<const Cell3DPosition, const Color> tCells; //!< the target cells as Cell/Color key-value pairs

    /**
     * @brief Add a cell to the target cells container
     * @param pos position of the target cell
     * @param c color of the cell. If none provided, defaults to (0,0,0,0)
     */
    void addTargetCell(const Cell3DPosition &pos, const Color c = Color());

    //!< @copydoc Target::print
    virtual void print(ostream& where) const;
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
    virtual bool isInTarget(const Cell3DPosition &pos) const;
    //!< @copydoc Target::getTargetColor
    //!< @throws InvalidPositionException is cell at position pos is not part of the target
    virtual const Color getTargetColor(const Cell3DPosition &pos);

    //!< @copydoc Target::BoundingBox
    virtual void boundingBox(BoundingBox &bb);

    /**
     * @brief Returns a list of all cells in target in ascending order (x, y, and then z)
     * @return the list of all cells in the target in ascending order
     */
    list<Cell3DPosition> getTargetCellsAsc();
};  // class TargetGrid

//<! @brief A target modeled as a container of unique positions using a coordinate system relative to some specific origin module, and colors
class RelativeTargetGrid : public TargetGrid {
     Cell3DPosition *origin = NULL;
public:
     //<! @brief Exception thrown if an there is an attempt to use the RelativeTargetGrid without having set its origin beforehand
     struct MissingInitializationException : std::exception {
          const char* what() const noexcept {
               return "Attempted to call isInTarget without having set the target's origin first\n";
          }
     };
protected:
RelativeTargetGrid(TiXmlNode *targetNode) : TargetGrid(targetNode) {};
     virtual ~RelativeTargetGrid() { delete origin; };

    //!< @copydoc Target::getTargetColor
    //!< a cell is in the target grid if and only if it is present in the target cells container//!< @warning Can only be used once origin has been set, and expects a relative position as input     
    virtual bool isInTarget(const Cell3DPosition &pos) const;

    /**
     * @brief Returns a list of all cells in target in ascending order (x, y, and then z)
     * @return the list of all cells in the target in ascending order
     * @warning Can only be used once origin has been set
     */
    list<Cell3DPosition> getTargetCellsAsc();

    /**
     * @brief Sets the origin of the coordinate system used by the target
     * @warning Calling Target::isInTarget before setting the origin will result in an error
     */    
    virtual void setOrigin(const Cell3DPosition &org);
};  // class RelativeTargetGrid

//<! @brief A target modeled as an ensemble of shapes
class TargetCSG : public Target {
    CSGNode *csgRoot;
    BoundingBox bb;

protected:
    //!< @copydoc Target::print
    virtual void print(ostream& where) const {};
public:
    TargetCSG(TiXmlNode *targetNode);
    virtual ~TargetCSG() {};

    //!< @copydoc Target::isInTarget
    virtual bool isInTarget(const Cell3DPosition &pos) const;
    //!< @copydoc Target::getTargetColor
    virtual const Color getTargetColor(const Cell3DPosition &pos);
    //!< @copydoc Target::boundingBox
    virtual void boundingBox(BoundingBox &bb);
    /**
     * @brief Grid to world position within bounding box
     * @param pos position of the target cell
     */
    Vector3D gridToWorldPosition(const Cell3DPosition &pos) const;
    /**
     * @brief The object is in the border of the target
     * @param pos position of the target cell
     * @param radius radius of the border
     */
    bool isInTargetBorder(const Cell3DPosition &pos, double radius) const;

};  // class TargetCSG

} // namespace BaseSimulator

#endif  // TARGET_H__
