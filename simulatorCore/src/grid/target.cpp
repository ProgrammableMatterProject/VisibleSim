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
            TiXmlElement* element = Target::targetNode->ToElement();
            const char *attr = element->Attribute("format");
            if (attr) {
                string str(attr);
                if (str.compare("grid") == 0) {
                    return new TargetGrid(Target::targetNode);
                } else if (str.compare("csg") == 0) {
                    return new TargetCSG(Target::targetNode);
                } else if (str.compare("relativeGrid") == 0) {
                    return new RelativeTargetGrid(Target::targetNode);
                } else if (str.compare("surface") == 0) {
                    return new TargetSurface(Target::targetNode);
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

ostream& operator<<(ostream& out,const Target *t) {
    t->print(out);
    return out;
}

/************************************************************
 *                      TargetGrid
 ************************************************************/

TargetGrid::TargetGrid(TiXmlNode *targetNode) : Target(targetNode) {
    TiXmlNode *cellNode = targetNode->FirstChild("cell");
    const char* attr;
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
            string str(attr);
            int pos1 = str.find_first_of(','),
                pos2 = str.find_last_of(',');
            position.pt[0] = atoi(str.substr(0,pos1).c_str());
            position.pt[1] = atoi(str.substr(pos1+1,pos2-pos1-1).c_str());
            position.pt[2] = atoi(str.substr(pos2+1,str.length()-pos1-1).c_str());
        } else {
            stringstream error;
            error << "position attribute missing for target cell" << "\n";
            throw ParsingException(error.str());
        }
        attr = element->Attribute("color");
        if (attr) {
            string str(attr);
            int pos1 = str.find_first_of(','),
                pos2 = str.find_last_of(',');
            color.set(atof(str.substr(0,pos1).c_str())/255.0,
                      atof(str.substr(pos1+1,pos2-pos1-1).c_str())/255.0,
                      atof(str.substr(pos2+1,str.length()-pos1-1).c_str())/255.0);
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
            string str(attr);
            int pos1 = str.find_first_of(','),
                pos2 = str.find_last_of(',');
            color.set(atof(str.substr(0,pos1).c_str())/255.0,
                      atof(str.substr(pos1+1,pos2-pos1-1).c_str())/255.0,
                      atof(str.substr(pos2+1,str.length()-pos1-1).c_str())/255.0);
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
            for(int i=0; i<n; i++) {
                if  (str[i] == '1') {
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

void TargetGrid::print(ostream& where) const {
    for(auto const& pair : tCells) {
        where << "<cell position=" << pair.first << " color=" << pair.second << " />" << endl;
    }
}
void TargetGrid::highlight() const {
    for (const auto& pair : tCells) {
        getWorld()->lattice->highlightCell(pair.first, pair.second);
    }
}

void TargetGrid::unhighlight() const {
    for (const auto& pair : tCells) {
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
    for (const auto& targetEntry : tCells) {
        absMap.insert(
            std::pair<const Cell3DPosition, const Color>(targetEntry.first + *origin,
                                                        targetEntry.second));
    }

    tCells = map<const Cell3DPosition, const Color>(absMap);

    computeGeodesics(); // Will populate each cell's distance to the origin in hops

    if (!targetCellsInConstructionOrder) {
        targetCellsInConstructionOrder = new list<Cell3DPosition>();

        for (const auto &pair : tCells) {
            cout << pair.first << " -dist: " << geodesicToOrigin[pair.first] << endl;
            targetCellsInConstructionOrder->push_back(pair.first);
        }

        targetCellsInConstructionOrder->
            sort([=](const Cell3DPosition& first, const Cell3DPosition& second){
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
    Lattice* lattice = static_cast<Lattice*>(getWorld()->lattice);

    while(!queue.empty()) {
        Cell3DPosition cell = queue.front();
        queue.pop_front();

        // Get all adjacent cells of dequeued cell.
        // If one of the adjacent cells has not been visited, mark
        //  it as visited and enqueue it
        for (const auto& nCell : lattice->getNeighborhood(cell))
        {
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

    for (const auto& cell : *targetCellsInConstructionOrder) {
        short distColorIdx = geodesicToOrigin.find(cell)->second % NB_COLORS;
        getWorld()->lattice->highlightCell(cell, Colors[distColorIdx]);
    }
}

list<Cell3DPosition>* RelativeTargetGrid::getTargetCellsInConstructionOrder() {
    if (!origin)
        throw MissingInitializationException();

    return targetCellsInConstructionOrder;
}

void RelativeTargetGrid::removeTargetCell(const Cell3DPosition& tc) {
    tCells.erase(tc);
}

bool RelativeTargetGrid::reconfigurationIsComplete() const { return tCells.empty(); }

void RelativeTargetGrid::relatifyAndPrint() {
    cout << endl << "=== START RELATIFIED TARGET ===" << endl << endl;

    const auto& minPair =
        *std::min_element(tCells.begin(), tCells.end(),
                          [](const std::pair<Cell3DPosition, Color>& pair1,
                             const std::pair<Cell3DPosition, Color>& pair2) {
                              return Cell3DPosition::compare_ZYX(pair1.first, pair2.first);
                          });

    for (const auto &pair : tCells) {
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
    bool boundingBox=true;
    element->QueryBoolAttribute("boundingBox", &boundingBox);
    offsetBoundingBox = false;
    element->QueryBoolAttribute("offset", &offsetBoundingBox);

    CSGParser parser;
    csgRoot = parser.parseCSG(str);
    csgRoot->toString();

    const char *attr = element->Attribute("translate");
    if (attr) {
        string str_attr(attr);
        int pos1 = str_attr.find_first_of(','),
            pos2 = str_attr.find_last_of(',');
        translate.pt[0] = atof(str_attr.substr(0,pos1).c_str());
        translate.pt[1] = atof(str_attr.substr(pos1+1,pos2-pos1-1).c_str());
        translate.pt[2] = atof(str_attr.substr(pos2+1,str_attr.length()-pos1-1).c_str());
    }

    if (boundingBox) csgRoot->boundingBox(bb);
}

Vector3D TargetCSG::gridToCSGPosition(const Cell3DPosition &pos) const {
    Vector3D res = getWorld()->lattice->gridToUnscaledWorldPosition(pos);

    if (offsetBoundingBox) {
        res.pt[0] += bb.P0[0] - 1.0 - translate.pt[0];
        res.pt[1] += bb.P0[1] - 1.0 - translate.pt[1];
        res.pt[2] += bb.P0[2] - 1.0 - translate.pt[2];
    } else {
        res.pt[0] += bb.P0[0] - translate.pt[0];
        res.pt[1] += bb.P0[1] - translate.pt[1];
        res.pt[2] += bb.P0[2] - translate.pt[2];
    }

    // cout << "gridToWorldPosition" << pos << " -> " << res << endl;

    return res;
}

Cell3DPosition TargetCSG::CSGToGridPosition(const Vector3D &pos) const {
    Vector3D unboundPos = pos;

    if (offsetBoundingBox) {
        unboundPos.pt[0] -= bb.P0[0] + 1.0 + translate.pt[0];
        unboundPos.pt[1] -= bb.P0[1] + 1.0 + translate.pt[1];
        unboundPos.pt[2] -= bb.P0[2] + 1.0 + translate.pt[2];
    } else {
        unboundPos.pt[0] -= bb.P0[0] + translate.pt[0];
        unboundPos.pt[1] -= bb.P0[1] + translate.pt[1];
        unboundPos.pt[2] -= bb.P0[2] + translate.pt[2];
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
        const Cell3DPosition& glb = lattice->getGridLowerBounds(iz);
        const Cell3DPosition& ulb = lattice->getGridUpperBounds(iz);
        for (short iy = glb[1]; iy <= ulb[1]; iy++) {
            for (short ix = glb[0]; ix <= ulb[0]; ix++) {
                p.set(ix,iy,iz);

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
        const Cell3DPosition& glb = lattice->getGridLowerBounds(iz);
        const Cell3DPosition& ulb = lattice->getGridUpperBounds(iz);
        for (short iy = glb[1]; iy <= ulb[1]; iy++) {
            for (short ix = glb[0]; ix <= ulb[0]; ix++) {
                p.set(ix,iy,iz);

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

/************************************************************
 *                      TargetSurface
 ************************************************************/

TargetSurface::TargetSurface(TiXmlNode *targetNode) : Target(targetNode) {
    TiXmlNode *methNode = targetNode->FirstChild("method");
    TiXmlElement *element;
    Cell3DPosition position;
    Color defaultColor = Color();
    Color color;

    if (methNode) {
            element = methNode->ToElement();
            const char *attr = element->Attribute("meth");
            if (attr) {
                string str(attr);
                if (str.compare("interpolation") == 0) {
                    method = str;
                    cout << "Method initialized" << method << endl;
                }
                else if (str.compare("neighbor") == 0) {
                    method = str;
                    cout << "Method initialized" << method << endl;
                }
                else if (str.compare("nurbs") == 0) {
                    method = str;
                    cout << "Method initialized" << method << endl;
                }
                TiXmlNode *cellNode = methNode->FirstChild("cell");
                // Parse individual cells
                while (cellNode) {
                element = cellNode->ToElement();
                color = defaultColor;

                attr = element->Attribute("position");
                if (attr) {
                    string str(attr);
                    int pos1 = str.find_first_of(','),
                        pos2 = str.find_last_of(',');
                    position.pt[0] = atoi(str.substr(0,pos1).c_str());
                    position.pt[1] = atoi(str.substr(pos1+1,pos2-pos1-1).c_str());
                    position.pt[2] = atoi(str.substr(pos2+1,str.length()-pos1-1).c_str());
                } else {
                    stringstream error;
                    error << "position attribute missing for target cell" << "\n";
                    throw ParsingException(error.str());
                }
                attr = element->Attribute("color");
                if (attr) {
                    string str(attr);
                    int pos1 = str.find_first_of(','),
                        pos2 = str.find_last_of(',');
                    color.set(atof(str.substr(0,pos1).c_str())/255.0,
                              atof(str.substr(pos1+1,pos2-pos1-1).c_str())/255.0,
                              atof(str.substr(pos2+1,str.length()-pos1-1).c_str())/255.0);
                }

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
                    string str(attr);
                    int pos1 = str.find_first_of(','),
                        pos2 = str.find_last_of(',');
                    color.set(atof(str.substr(0,pos1).c_str())/255.0,
                              atof(str.substr(pos1+1,pos2-pos1-1).c_str())/255.0,
                              atof(str.substr(pos2+1,str.length()-pos1-1).c_str())/255.0);
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
                    for(int i=0; i<n; i++) {
                        if  (str[i] == '1') {
                            position.pt[0] = i;
                            addTargetCell(position, color);
                        }
                    }
                }

                cellNode = cellNode->NextSibling("blocksLine");
            } // end while (cellNode)*/
            }
    }


    if (method.compare("interpolation") == 0) {
        //Calculate polynom coeff
        cout << "Coefficients to be calculated"<< endl;
        //Calculation of d polynom degree
        int d = 0;
        while ((int)pcl.size() > (d+1)*(d+2)/2){
            d = d+1;
        }
        if ((int)pcl.size() < (d+1)*(d+2)/2){
            d = d-1;
        }
        cout << pcl.size() << "points in the cloud" << endl;
        cout << "Polynom bivariate of degree " << d << endl;
        //Initialization of n number of coefficients
        int n = (d+1)*(d+2)/2;
        //Placing all z coordinate in a vector
        vector<float> z;
        for (int i=0;i<n;i++){
            z.push_back(pcl[i].pt[2]);
        }
        for (int i=0;i<n;i++){
            cout << "z" << i <<" = " << z[i] << endl;
        }
        //Creating the matrix for coeff calculation
        Eigen::MatrixXf m(n,n);
        for (int i=0;i<n;i++){
            float x = pcl[i].pt[0];
            float y = pcl[i].pt[1];
            int j = 0;
            for (int k=0;k<=d;k++){
                for(int l=0;l<=(d-k);l++){
                    m(i,j)=pow(x,(d-k-l))*pow(y,l);
                    j=j+1;
                }
            }
        }
        cout << m << endl;
        //Determinant calculations
        cout << "determinant de M =" << m.determinant() << endl;
        vector<vector<float>> det;
        for (int k=0;k<n;k++){
            vector<float> detcoef;
            for (int l=0;l<n;l++){
                Eigen::MatrixXf Mkl(n-1,n-1);
                Mkl.topLeftCorner(k,l) = m.topLeftCorner(k,l);
                Mkl.topRightCorner(k,n-l-1) = m.topRightCorner(k,n-l-1);
                Mkl.bottomLeftCorner(n-k-1,l) = m.bottomLeftCorner(n-k-1,l);
                Mkl.bottomRightCorner(n-k-1,n-l-1) = m.bottomRightCorner(n-k-1,n-l-1);
                detcoef.push_back(pow(-1,k+l)*Mkl.determinant());
            }
            det.push_back(detcoef);
        }
        for (int i=0;i<n;i++){
            cout << "determinant de M" << i << endl;
            for (int j=0;j<n;j++){
                cout << "coeff " << j << " de M" << i << " = " << det[i][j] << endl;
            }
        }
        //Polynom coefficient calculation
        for (int i=0;i<n;i++){
            float coeff=0;
            for (int j=0;j<n;j++){
                coeff = coeff+((z[j]*det[j][i])/m.determinant());
            }
            coeffs.push_back(coeff);
        }
        for (int i=0;i<n;i++){
            cout << "Coeff " << i << " = " << coeffs[i] << endl;
        }
    }

    if (method.compare("nurbs") == 0) {
        cout << "Call to the NURBS surface type initialization" << endl;
        //Initialization of the NURBS parameters, parsing from config file to be implemented later

        float scale = 1;

        TiXmlNode *scaleNode = methNode->FirstChild("scale");
            if (scaleNode) {
                element = scaleNode->ToElement();
                const char *attr = element->Attribute("scale");
                if (attr) {
                    string str(attr);
                    scale = atof(str.c_str());
                }
            }


        TiXmlNode *sorderNode = methNode->FirstChild("sorder");
            if (sorderNode) {
                element = sorderNode->ToElement();
                const char *attr = element->Attribute("order");
                if (attr) {
                    string str(attr);
                    S_ORDER = atoi(str.c_str());
                }
            }

            TiXmlNode *sknotNode = methNode->FirstChild("sknot");
            while (sknotNode) {
                element = sknotNode->ToElement();
                const char *attr = element->Attribute("knot");
                if (attr) {
                    string str(attr);
                    sknots.push_back(atof(str.c_str()));
                }
                sknotNode = sknotNode->NextSibling("sknot");
            }

            S_NUMKNOTS = sknots.size();
            S_NUMPOINTS = S_NUMKNOTS-S_ORDER;

            TiXmlNode *torderNode = methNode->FirstChild("torder");
            if (torderNode) {
                element = torderNode->ToElement();
                const char *attr = element->Attribute("order");
                if (attr) {
                    string str(attr);
                    T_ORDER = atoi(str.c_str());
                }
            }

            TiXmlNode *tknotNode = methNode->FirstChild("tknot");
            while (tknotNode) {
                element = tknotNode->ToElement();
                const char *attr = element->Attribute("knot");
                if (attr) {
                    string str(attr);
                    tknots.push_back(atof(str.c_str()));
                }
                tknotNode = tknotNode->NextSibling("tknot");
            }

            T_NUMKNOTS = tknots.size();
            T_NUMPOINTS = T_NUMKNOTS-T_ORDER;

            TiXmlNode *ctlpointNode = methNode->FirstChild("ctlpoint");
            vector<vector<float>> tctlpoint;
            vector<float> point;
            while (ctlpointNode) {
                element = ctlpointNode->ToElement();
                const char *attr = element->Attribute("coord");
                if (attr) {
                    string str(attr);
                    int pos1 = str.find_first_of(',');
                    int pos3 = str.find_last_of(',');
                    str.replace(pos1,1,".");
                    int pos2 = str.find_first_of(',');
                    point.push_back(scale*atof(str.substr(0,pos1).c_str()));
                    point.push_back(scale*atof(str.substr(pos1+1,pos2-pos1-1).c_str()));
                    point.push_back(scale*atof(str.substr(pos2+1,pos3-pos2-1).c_str()));
                    point.push_back(scale*atof(str.substr(pos3+1).c_str()));
                    tctlpoint.push_back(point);
                    point.clear();
                }
                if ((int)tctlpoint.size() == T_NUMPOINTS){
                    ctlpoints.push_back(tctlpoint);
                    tctlpoint.clear();
                }
                ctlpointNode = ctlpointNode->NextSibling("ctlpoint");
            }



        //Checking if sknots correctly filled

        for (int i=0; i < S_NUMKNOTS; i++){
            cout << "sknots[" << i << "] = " << sknots[i] << endl;
        }
        //Checking if tknots correctly filled

        for (int i=0; i < T_NUMKNOTS; i++){
            cout << "tknots[" << i << "] = " << tknots[i] << endl;
        }

        //Checking ctlpoints

        for (int i=0; i < S_NUMPOINTS; i++){
            for (int j=0; j < T_NUMPOINTS; j++){
                cout << "ctlpoints["<<i<<"]["<<j<<"][2] = "<< ctlpoints[i][j][2]<< endl;
             }
        }



        /*
        float_sknots = new float[S_NUMKNOTS];
        float_tknots = new float[T_NUMKNOTS];
        float_ctlpoints = new float**[S_NUMPOINTS];

        for (int i=0; i<S_NUMKNOTS; i++) {
            float_sknots[i] = sknots[i];
            cout << "float_sknots[" << i << "]=" << float_sknots[i] << endl;
        }
        for (int i=0; i<T_NUMKNOTS; i++) {
            float_tknots[i] = tknots[i];
            cout << "float_tknots[" << i << "]=" << float_tknots[i] << endl;
        }
        for (int i=0; i<S_NUMPOINTS; i++) {
            float_ctlpoints[i] = new float*[T_NUMPOINTS];
            for(int j=0; j<T_NUMPOINTS; j++) {
                float_ctlpoints[i][j] = new float[4];
                for (int k=0; k<4; k++) {
                    float_ctlpoints[i][j][k] = ctlpoints[i][j][k];
                    cout << "float_ctlpoints[" << i << "]["<<j<<"]["<<k<<"]=" << float_ctlpoints[i][j][k]<< endl;
                }
            }
        }


        theNurb = gluNewNurbsRenderer();
        gluNurbsProperty(theNurb, GLU_SAMPLING_TOLERANCE, 50.0);
        gluNurbsProperty(theNurb, GLU_DISPLAY_MODE, GLU_FILL);*/
        //NURBS parameters initialization finished
    }

}

bool TargetSurface::isInTarget(const Cell3DPosition &pos) const {
    //Initialization
    Vector3D cartesianpos = getWorld()->lattice->gridToWorldPosition(pos);
    float x = cartesianpos.pt[0];
    float y = cartesianpos.pt[1];
    float z = cartesianpos.pt[2];

    if (method.compare("interpolation") == 0) {
        //Calculation of d polynom degree
        int d = 0;
        while ((int)coeffs.size()>(d+1)*(d+2)/2) {
            d = d+1;
        }
        if ((int)coeffs.size()<(d+1)*(d+2)/2) {
            d = d-1;
        }
        //Calculation of f(x,y)
        float f = 0;
        int j = 0;
        for (int k=0;k<=d;k++){
            for (int l=0;l<=(d-k);l++){
                f = f+coeffs[j]*pow(x,(d-k-l))*pow(y,l);
                j = j+1;
            }
        }
        //Comparison between f and z
        //cout << "x=" << x << ",y=" << y << ",z=" << z <<  " f= " << f << endl;
        if (z<=f){
            //cout << "x=" << x << ",y=" << y << " f= " << f << endl;
            return true;
        }
        else {
            return false;
        }
    }

    else if (method.compare("neighbor") == 0) {
        //print method
        cout << "Call to isInTarget method =" << method << endl;
        //Nearest neighboor research
        float mindist = FLT_MAX;
        int neighbor = 0;
        for (int i=0;i<(int)pcl.size();i++){
            float xi = pcl[i].pt[0];
            float yi = pcl[i].pt[1];
            float dist = pow((x-xi),2)+pow((y-yi),2);
            if (dist<mindist){
                mindist = dist;
                neighbor = i;
            }
        }
        //Comparison between z
        if (z<=pcl[neighbor].pt[2]){
            return true;
        }
        else{
            return false;
        }
    }


    else if (method.compare("nurbs") == 0) {
        //print method
        //cout << "Call to isInTarget method =" << method << endl;
        //Initialization of dichotomy parameters
        float precision = FLT_MAX; (void)precision;
        //float precGoal = 100;
        float approx = 0.01;
        float u0 = 0;// + approx;
        float u1 = 1;// - approx;
        float v0 = 0;// + approx;
        float v1 = 1;// - approx;
        float znurbs = 0;
        int count = 0;

        //Begin dichotomy process
        while (count<10){
        // || precision>precGoal || ((u0 == u1) && (v0 == v1))){
            float mindist = FLT_MAX;
            int quadrant = 0;
            if (count == 0){
                //cout << "x1=" <<x1<<",x2="<<x2<<",x3="<<x3<<",x4="<<x4<<endl;
                //cout << "y1=" <<y1<<",y2="<<y2<<",y3="<<y3<<",y4="<<y4<<endl;
                float x1 = calculateNurbs(u0+approx,v0+approx,0);
                float y1 = calculateNurbs(u0+approx,v0+approx,1);
                float x2 = calculateNurbs(u1-approx,v0+approx,0);
                float y2 = calculateNurbs(u1-approx,v0+approx,1);
                float x3 = calculateNurbs(u0+approx,v1-approx,0);
                float y3 = calculateNurbs(u0+approx,v1-approx,1);
                float x4 = calculateNurbs(u1-approx,v1-approx,0);
                float y4 = calculateNurbs(u1-approx,v1-approx,1);
                if ( x1 < x && x2 < x && x3 < x && x4 < x ){
                    return false;
                }
                if ( y1 < y && y2 < y && y3 < y && y4 < y ){
                    return false;
                }
                if ( x1 > x && x2 > x && x3 > x && x4 > x ){
                    return false;
                }
                if ( y1 > y && y2 > y && y3 > y && y4 > y ){
                    return false;
                }
            }

            float u = (u0+u1)/2;
            float v = (v0+v1)/2;
            float x1 = calculateNurbs(u-approx,v-approx,0);
            float y1 = calculateNurbs(u-approx,v-approx,1);
            float x2 = calculateNurbs(u+approx,v-approx,0);
            float y2 = calculateNurbs(u+approx,v-approx,1);
            float x3 = calculateNurbs(u-approx,v+approx,0);
            float y3 = calculateNurbs(u-approx,v+approx,1);
            float x4 = calculateNurbs(u+approx,v+approx,0);
            float y4 = calculateNurbs(u+approx,v+approx,1);

            //Search quadrant (x,y) is in
            if (dist(x1,y1,x,y)<mindist){
                mindist = dist(x1,y1,x,y);
                quadrant = 1;
            }
            if (dist(x2,y2,x,y)<mindist){
                mindist = dist(x2,y2,x,y);
                quadrant = 2;
            }
            if (dist(x3,y3,x,y)<mindist){
                mindist = dist(x3,y3,x,y);
                quadrant = 3;
            }
            if (dist(x4,y4,x,y)<mindist){
                mindist = dist(x4,y4,x,y);
                quadrant = 4;
            }
            //Update dichotomy parameters and znurbs
            if (quadrant == 1){
//                cout << "my x="<<x<<" my y="<<y<<" x found="<<x1<<" y found="<<y1<<endl;
                znurbs = calculateNurbs(u0,v0,2);
                u1 = u;//(u1+u0)/2;
                v1 = v;//(v1+v0)/2;
            }
            if (quadrant == 2){
//                cout << "my x="<<x<<" my y="<<y<<" x found="<<x2<<" y found="<<y2<<endl;
                znurbs = calculateNurbs(u1,v0,2);
                u0 = u;//(u1+u0)/2;
                v1 = v;//(v1+v0)/2;
            }
            if (quadrant == 3){
//                cout << "my x="<<x<<" my y="<<y<<" x found="<<x3<<" y found="<<y3<<endl;
                znurbs = calculateNurbs(u0,v1,2);
                u1 = u;//(u1+u0)/2;
                v0 = v;//(v1+v0)/2;
            }
            if (quadrant == 4){
//                cout << "my x="<<x<<" my y="<<y<<" x found="<<x4<<" y found="<<y4<<endl;
                znurbs = calculateNurbs(u1,v1,2);
                u0 = u;//(u1+u0)/2;
                v0 = v;//(v1+v0)/2;
            }

            precision = sqrt(mindist);
/*
            if (count < 10){
                cout << "quadrant =" << quadrant << endl;
                cout << "u0 = " << u0 <<endl;
                cout << "v0 = " << v0 << endl;
                cout << "u1 = " << u1 << endl;
                cout << "v1 = " << v1 << endl;
                cout << "precision =" <<  precision << endl;
            }
*/
            //cout<<"quadrant="<<quadrant<<endl;
            //cout<<"x="<<x<<",x1="<<x1<<",x2="<<x2<<",x3="<<x3<<",x4="<<x4<<endl;
            //cout<<"y="<<y<<",y1="<<y1<<",y2="<<y2<<",y3="<<y3<<",y4="<<y4<<endl;
            count += 1;
        }
        //cout << "End of while: znurbs=" << znurbs << " ,précision =" << precision << endl;
        //Comparison between z
/*        float znurbs=0;

       for (int i=0;i<11;i++){
            for (int j=0;j<11;j++){
                for (int k=0;k<3;k++){
                    cout << "S(" << i*0.1 <<","<<j*0.1<<")."<<k<<"="<<calculateNurbs(i*0.1,j*0.1,k)<<endl;
                }
            }
        }
*/
        if (z<=znurbs){
            return true;
        }
        else{
            return false;
        }
    }

    else {
        throw BaseSimulator::NotImplementedException();
    }
}

const Color TargetSurface::getTargetColor(const Cell3DPosition &pos) const {
    throw BaseSimulator::NotImplementedException("TargetSurface::getTargetColor");
}

void TargetSurface::addTargetCell(const Cell3DPosition &pos, const Color c) {
    Vector3D v;
    v.set(pos.pt[0],pos.pt[1],pos.pt[2],1);
    pcl.push_back(v);
    cout << "Point ("<<v.pt[0]<<","<<v.pt[1]<<","<<v.pt[2]<<") added to pointCloud" << endl;
}


float TargetSurface::calculateNurbs(float u, float v, int coord) const {

    //cout<<"u="<<u<<endl;
    //cout<<"v="<<v<<endl;

    //Basis function on first variable calculation
    float Ni[S_NUMKNOTS-1][S_ORDER];
    for (int i=0; i<S_NUMKNOTS-1; i++){
        Ni[i][0]=0;
        if ( sknots[i]<= u && u < sknots[i+1]){
            Ni[i][0]=1;
            //cout << "Ni["<<i<<"][0]=1"<<endl;
        }
        //cout<<"Ni["<<i<<"][0]("<<u<<")="<<Ni[i][0]<<endl;
    }
    for (int d=1; d < S_ORDER; d++){
        for (int i=0; i < S_NUMKNOTS-1-d; i++){
            float a;
            float b;
            if ( (u-sknots[i])*Ni[i][d-1] == 0 && (sknots[i+d]-sknots[i])==0){
                a=0;
            }
            else {
                a = ((u-sknots[i])*Ni[i][d-1])/(sknots[i+d]-sknots[i]);
                //cout<<"a="<<a<<endl;
            }
            if ( (sknots[i+d+1]-u)*Ni[i+1][d-1]==0 && (sknots[i+d+1]-sknots[i+1])==0){
                b=0;
            }
            else {
                b = ((sknots[i+d+1]-u)*Ni[i+1][d-1])/(sknots[i+d+1]-sknots[i+1]);
                //cout<<"b="<<b<<endl;
            }
            Ni[i][d]=a+b;
            //cout<<"Ni["<<i<<"]["<<d<<"]("<<u<<")="<<Ni[i][d]<<endl;
        }
    }
    //Basis function on second variable calculation
    float Nj[T_NUMKNOTS-1][T_ORDER];
    for (int j=0; j<T_NUMKNOTS-1; j++){
        Nj[j][0]=0;
        if ( tknots[j]<= v && v < tknots[j+1]){
            Nj[j][0]=1;
            //cout << "Nj["<<j<<"][0]=1"<<endl;
        }
        //cout<<"Nj["<<j<<"][0]("<<v<<")="<<Nj[j][0]<<endl;
    }
    for (int d=1; d < T_ORDER; d++){
        for (int j=0; j < T_NUMKNOTS-1-d; j++){
            float a;
            float b;
            if ( (v-tknots[j])*Nj[j][d-1] == 0 && (tknots[j+d]-tknots[j])==0){
                a=0;
            }
            else {
                a = ((v-tknots[j])*Nj[j][d-1])/(tknots[j+d]-tknots[j]);
                //cout<<"a="<<a<<endl;
            }
            if ( (tknots[j+d+1]-v)*Nj[j+1][d-1]==0 && (tknots[j+d+1]-tknots[j+1])==0){
                b=0;
            }
            else {
                b = ((tknots[j+d+1]-v)*Nj[j+1][d-1])/(tknots[j+d+1]-tknots[j+1]);
                //cout<<"b="<<b<<endl;
            }
            Nj[j][d]=a+b;
            //cout<<"Nj["<<j<<"]["<<d<<"]("<<v<<")="<<Nj[j][d]<<endl;
        }
    }
    //S calculation
    float num = 0;
    float den = 0;
    //for (int i =0; i <= S_NUMPOINTS-S_ORDER; i++){
        //for (int j=0; j <= T_NUMPOINTS-T_ORDER; j++){
    for (int i =0; i < S_NUMPOINTS; i++){
        for (int j=0; j < T_NUMPOINTS; j++){
            //num = num + Ni[i][S_ORDER-1]*Nj[j][T_ORDER-1]*ctlpoints[i][j][3]*ctlpoints[i][j][coord];
            //den = den + Ni[i][S_ORDER-1]*Nj[j][T_ORDER-1]*ctlpoints[i][j][3];
            num = num + Ni[i][S_ORDER-1]*Nj[j][T_ORDER-1]*ctlpoints[i][j][3]*ctlpoints[i][j][coord];
            den = den + Ni[i][S_ORDER-1]*Nj[j][T_ORDER-1]*ctlpoints[i][j][3];
        }
    }
    float S = num/den;
    //cout << "u =" << u << endl;
    //cout << "v =" << v << endl;
    //cout << "num =" << num << endl;
    //cout << "den =" << den << endl;
    //cout << "S =" << S << endl;
    return S;
}

float TargetSurface::dist(float x1, float y1, float x2, float y2) const {
    float distance = (x2-x1)*(x2-x1)+(y2-y1)*(y2-y1);
    //cout << "dist = " << distance << endl;
    return distance;
}

void TargetSurface::print(ostream& where) const {

    if (method.compare("interpolation") == 0) {
        //print method
        cout << "Call to print method =" << method << endl;
    }

    else if (method.compare("neighbor") == 0) {
        //print method
        cout << "Call to print method =" << method << endl;
    }

    else if (method.compare("nurbs") == 0) {
        //print method
        cout << "Call to print method =" << method << endl;
    }

    else {
        throw BaseSimulator::NotImplementedException("TargetSurface::print");
    }
}

void TargetSurface::glDraw() {
    GLfloat mat_ambient[] = { 1.0, 1.0, 1.0, 1.0 };
    GLfloat mat_diffuse[] = { 1.0, 0.2, 1.0, 1.0 };
    GLfloat mat_specular[] = { 1.0, 1.0, 1.0, 1.0 };
    GLfloat mat_shininess[] = { 50.0 };

    //GLfloat light0_position[] = { 2.0, 0.1, 5.0, 0.0 };
    //GLfloat light1_position[] = { -2.0, 0.1, 5.0, 0.0 };
    //GLfloat lmodel_ambient[] = { 0.3, 0.3, 0.3, 1.0 };

    glMaterialfv(GL_FRONT, GL_AMBIENT, mat_ambient);
    glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_diffuse);
    glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
    glMaterialfv(GL_FRONT, GL_SHININESS, mat_shininess);

    glPushMatrix();
    gluBeginSurface(theNurb);
    gluNurbsSurface(theNurb,
        S_NUMKNOTS, float_sknots,
        T_NUMKNOTS, float_tknots,
        4 * T_NUMPOINTS,
        4,
        &float_ctlpoints[0][0][0],
        S_ORDER, T_ORDER,
        GL_MAP2_VERTEX_4);
    gluEndSurface(theNurb);
    glPopMatrix();
}


} // namespace BaseSimulator
