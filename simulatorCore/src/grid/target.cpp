/*! @file target.cpp01??* @brief Defines a target configuration for reconfiguration  algorithms,
 * several ways of defining the configuration are provided to the user.
 * @author Pierre Thalamy
 * @date 21/07/2016
 */
#include "target.h"
#include "../utils/utils.h"
#include "../csg/csgParser.h"
#include "../base/world.h"
#include "../deps/Eigen/Dense"
#include "base/simulator.h"

#include <algorithm>

namespace BaseSimulator {

    using namespace BaseSimulator::utils;

    TiXmlNode *Target::targetListNode = NULL;
    TiXmlNode *Target::targetNode = NULL;

    Target *Target::loadNextTarget() {
        if (Target::targetListNode) {
            // Move targetNode pointer to next target (or NULL if there is none)
            Target::targetNode = targetListNode->IterateChildren(targetNode);

            if (Target::targetNode) {
                TiXmlElement *element = Target::targetNode->ToElement();
                const char *attr = element->Attribute("format");
                if (attr) {
                    string str(attr);
                    if (str.compare("grid") == 0) {
                        return new TargetGrid(Target::targetNode);
                    } else if (str.compare("csg") == 0) {
                        return new TargetCSG(Target::targetNode);
                    } else if (str.compare("relativeGrid") == 0) {
                        return new RelativeTargetGrid(Target::targetNode);
                    }
                } else {
                    throw UnknownTargetFormatException(attr);
                }
            }
        }


        return NULL;
    }

    void Target::glDraw() {
    }

/************************************************************
 *                         Target
 ************************************************************/

    ostream &operator<<(ostream &out, const Target *t) {
        t->print(out);
        return out;
    }

/************************************************************
 *                      TargetGrid
 ************************************************************/

    TargetGrid::TargetGrid(TiXmlNode *targetNode) : Target(targetNode) {
        TiXmlNode *cellNode = targetNode->FirstChild("cell");
        const char *attr;
        TiXmlElement *element;
        Cell3DPosition position;
        Color defaultColor = Color();
        Color color;

        // Parse individual cells
        while (cellNode) {
            element = cellNode->ToElement();
            color = defaultColor;

            attr = element->Attribute("position");
            if (attr) {
                position = BaseSimulator::Simulator::extractCell3DPositionFromString(attr);
            } else {
                stringstream error;
                error << "position attribute missing for target cell" << "\n";
                throw ParsingException(error.str());
            }
            attr = element->Attribute("color");
            if (attr) {
                color = BaseSimulator::Simulator::extractColorFromString(attr);
            }
            OUTPUT << "add target " << position << "," << color << endl;
            addTargetCell(position, color);
            cellNode = cellNode->NextSibling("cell");
        } // end while (cellNode)

        // Parse lines of cells
        cellNode = targetNode->FirstChild("targetLine");
        while (cellNode) {
            int line = 0, plane = 0;
            element = cellNode->ToElement();
            color = defaultColor;
            attr = element->Attribute("color");
            if (attr) {
                color = BaseSimulator::Simulator::extractColorFromString(attr);
            }

            attr = element->Attribute("line");
            if (attr) {
                line = atoi(attr);
            }

            attr = element->Attribute("plane");
            if (attr) {
                plane = atoi(attr);
            }

            attr = element->Attribute("values");
            if (attr) {
                string str(attr);
                position.pt[0] = 0;
                position.pt[1] = line;
                position.pt[2] = plane;
                int n = str.length();
                for (int i = 0; i < n; i++) {
                    if (str[i] == '1') {
                        position.pt[0] = i;
                        addTargetCell(position, color);
                    }
                }
            }

            cellNode = cellNode->NextSibling("blocksLine");
        } // end while (cellNode)*/
    }

    bool TargetGrid::isInTarget(const Cell3DPosition &pos) const {
        return tCells.count(pos);
    }

    const Color TargetGrid::getTargetColor(const Cell3DPosition &pos) const {
        if (!isInTarget(pos) or tCells.find(pos) == tCells.end()) {
            cerr << "error: attempting to get color of undefined target cell" << endl;
            throw InvalidPositionException(pos);
        }

        return tCells.find(pos)->second;
    }

    void TargetGrid::addTargetCell(const Cell3DPosition &pos, const Color c) {
        tCells.insert(std::pair<const Cell3DPosition, const Color>(pos, c));
    }

    void TargetGrid::print(ostream &where) const {
        for (auto const &pair: tCells) {
            where << "<cell position=" << pair.first << " color=" << pair.second << " />" << endl;
        }
    }

    void TargetGrid::highlight() const {
        Color c;
        for (const auto &pair: tCells) {
            getWorld()->lattice->highlightCell(pair.first, pair.second);
        }
    }

    void TargetGrid::unhighlight() const {
        for (const auto &pair: tCells) {
            getWorld()->lattice->unhighlightCell(pair.first);
        }
    }

/************************************************************
 *                      RelativeTargetGrid
 ************************************************************/

    bool RelativeTargetGrid::isInTarget(const Cell3DPosition &pos) const {
        if (!origin)
            throw MissingInitializationException();

        return TargetGrid::isInTarget(pos);
    }

    void RelativeTargetGrid::setOrigin(const Cell3DPosition &org) {
        assert(!tCells.empty());

        // relatifyAndPrint();
        origin = new Cell3DPosition(org);

        // Then update every relative position parsed from the configuration file to its absolute counterpart
        map<const Cell3DPosition, const Color> absMap;
        for (const auto &targetEntry: tCells) {
            absMap.insert(
                    std::pair<const Cell3DPosition, const Color>(targetEntry.first + *origin,
                                                                 targetEntry.second));
        }

        tCells = map<const Cell3DPosition, const Color>(absMap);

        computeGeodesics(); // Will populate each cell's distance to the origin in hops

        if (!targetCellsInConstructionOrder) {
            targetCellsInConstructionOrder = new list<Cell3DPosition>();

            for (const auto &pair: tCells) {
                cout << pair.first << " -dist: " << geodesicToOrigin[pair.first] << endl;
                targetCellsInConstructionOrder->push_back(pair.first);
            }

            targetCellsInConstructionOrder->
                    sort([=](const Cell3DPosition &first, const Cell3DPosition &second) {
                // return geodesicToOrigin[first] < geodesicToOrigin[second];
                // if (first.dist_euclid(*origin) < second.dist_euclid(*origin))
                if (geodesicToOrigin[first] < geodesicToOrigin[second])
                    return true;
                    // else if (first.dist_euclid(*origin) > second.dist_euclid(*origin))
                else if (geodesicToOrigin[first] > geodesicToOrigin[second])
                    return false;
                else {
                    if (first[0] < second[0]) return true;
                    else if (first[0] > second[0]) return false;
                    else {
                        if (first[1] > second[1]) return true;
                        else if (first[1] < second[1]) return false;
                        else {
                            return first[2] < second[2];
                        }
                    }
                }
            });
        }
    }

    void RelativeTargetGrid::computeGeodesics() {
        geodesicToOrigin[*origin] = 0;

        // BFS-parent of every connector
        std::map<Cell3DPosition, Cell3DPosition> parent;
        parent[*origin] = *origin;

        list<Cell3DPosition> queue;
        queue.push_back(*origin);

        list<Cell3DPosition>::iterator itCell;
        Lattice *lattice = static_cast<Lattice *>(getWorld()->lattice);

        while (!queue.empty()) {
            Cell3DPosition cell = queue.front();
            queue.pop_front();

            // Get all adjacent cells of dequeued cell.
            // If one of the adjacent cells has not been visited, mark
            //  it as visited and enqueue it
            for (const auto &nCell: lattice->getNeighborhood(cell)) {
                if (isInTarget(nCell) && parent.find(nCell) == parent.end()) {
                    parent[nCell] = cell;
                    geodesicToOrigin[nCell] = geodesicToOrigin[cell] + 1;

                    queue.push_back(nCell);
                }
            }
        }
    }

    void RelativeTargetGrid::highlightByDistanceToRoot() const {
        if (!origin)
            throw MissingInitializationException();

        for (const auto &cell: *targetCellsInConstructionOrder) {
            short distColorIdx = geodesicToOrigin.find(cell)->second % NB_COLORS;
            getWorld()->lattice->highlightCell(cell, Colors[distColorIdx]);
        }
    }

    list<Cell3DPosition> *RelativeTargetGrid::getTargetCellsInConstructionOrder() {
        if (!origin)
            throw MissingInitializationException();

        return targetCellsInConstructionOrder;
    }

    void RelativeTargetGrid::removeTargetCell(const Cell3DPosition &tc) {
        tCells.erase(tc);
    }

    bool RelativeTargetGrid::reconfigurationIsComplete() const { return tCells.empty(); }

    void RelativeTargetGrid::relatifyAndPrint() {
        cout << endl << "=== START RELATIFIED TARGET ===" << endl << endl;

        const auto &minPair =
                *std::min_element(tCells.begin(), tCells.end(),
                                  [](const std::pair<Cell3DPosition, Color> &pair1,
                                     const std::pair<Cell3DPosition, Color> &pair2) {
                                      return Cell3DPosition::compare_ZYX(pair1.first, pair2.first);
                                  });

        for (const auto &pair: tCells) {
            Cell3DPosition relCell = pair.first - minPair.first;
            cout << "<cell position=\"" << relCell.config_print() << "\" />" << endl;
        }

        cout << endl << "=== END RELATIFIED TARGET ===" << endl << endl;
    }

/************************************************************
 *                      TargetCSG
 ************************************************************/

    TargetCSG::TargetCSG(TiXmlNode *targetNode) : Target(targetNode) {
        TiXmlNode *cellNode = targetNode->FirstChild("csg");
        TiXmlElement *element = cellNode->ToElement();
        string str = element->Attribute("content");
        bool boundingBox = true;
        element->QueryBoolAttribute("boundingBox", &boundingBox);
        offsetBoundingBox = false;
        element->QueryBoolAttribute("offset", &offsetBoundingBox);

        CSGParser parser;
        csgRoot = parser.parseCSG(str);
        csgRoot->toString();

        const char *attr = element->Attribute("translate");
        if (attr) {
            translate = BaseSimulator::Simulator::extractVector3DFromString(attr);
        }

        if (boundingBox) csgRoot->boundingBox(bb);
    }

    Vector3D TargetCSG::gridToCSGPosition(const Cell3DPosition &pos) const {
        Vector3D res = getWorld()->lattice->gridToUnscaledWorldPosition(pos);

        res += bb.P0 - translate;
        if (offsetBoundingBox) {
            res -= Vector3D(1, 1, 1);
        }

        // cout << "gridToWorldPosition" << pos << " -> " << res << endl;
        return res;
    }

    Cell3DPosition TargetCSG::CSGToGridPosition(const Vector3D &pos) const {
        Vector3D unboundPos = pos;

        unboundPos -= bb.P0 + translate;
        if (offsetBoundingBox) {
            unboundPos -= Vector3D(1, 1, 1);
        }

        Cell3DPosition res = getWorld()->lattice->unscaledWorldToGridPosition(unboundPos);
        return res;
    }

    bool TargetCSG::isInTarget(const Cell3DPosition &pos) const {
        Color color;
        return csgRoot->isInside(gridToCSGPosition(pos), color);
    }

    bool TargetCSG::isInTargetBorder(const Cell3DPosition &pos, double radius) const {
        Color color;

        // cout << endl << "\nisInTargetBorder:pos: " << pos << "\t";
        return csgRoot->isInBorder(gridToCSGPosition(pos), color, radius);
    }

    void TargetCSG::boundingBox(BoundingBox &bb) {
        csgRoot->boundingBox(bb);
    }

    void TargetCSG::highlight() const {
        Lattice *lattice = BaseSimulator::getWorld()->lattice;

        Cell3DPosition p;
        for (short iz = 0; iz <= lattice->getGridUpperBounds()[2]; iz++) {
            const Cell3DPosition &glb = lattice->getGridLowerBounds(iz);
            const Cell3DPosition &ulb = lattice->getGridUpperBounds(iz);
            for (short iy = glb[1]; iy <= ulb[1]; iy++) {
                for (short ix = glb[0]; ix <= ulb[0]; ix++) {
                    p.set(ix, iy, iz);

                    Color color = WHITE;
                    if (csgRoot->isInside(gridToCSGPosition(p), color))
                        lattice->highlightCell(p, color);
                }
            }
        }
    }

    void TargetCSG::unhighlight() const {
        Lattice *lattice = BaseSimulator::getWorld()->lattice;

        Cell3DPosition p;
        for (short iz = 0; iz <= lattice->getGridUpperBounds()[2]; iz++) {
            const Cell3DPosition &glb = lattice->getGridLowerBounds(iz);
            const Cell3DPosition &ulb = lattice->getGridUpperBounds(iz);
            for (short iy = glb[1]; iy <= ulb[1]; iy++) {
                for (short ix = glb[0]; ix <= ulb[0]; ix++) {
                    p.set(ix, iy, iz);

                    Color c;
                    if (csgRoot->isInside(gridToCSGPosition(p), c))
                        lattice->unhighlightCell(p);
                }
            }
        }
    }

    const Color TargetCSG::getTargetColor(const Cell3DPosition &pos) const {
        Color color = WHITE;

        if (!csgRoot->isInside(gridToCSGPosition(pos), color)) {
            cerr << "error: attempting to get color of undefined target cell" << endl;
            throw InvalidPositionException(pos);
        }

        return color;
    }

    void TargetCSG::glDraw() {
        csgRoot->glDraw();
    }


} // namespace BaseSimulator
