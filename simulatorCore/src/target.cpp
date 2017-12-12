/*! @file target.cpp
 * @brief Defines a target configuration for reconfiguration algorithms, 
 * several ways of defining the configuration are provided to the user.
 * @author Pierre Thalamy
 * @date 21/07/2016
 */

#include "target.h"
#include "utils.h"
#include "csgParser.h"
#include "csgUtils.h"
#include "csg.h"
#include "catoms3DWorld.h"

namespace BaseSimulator {

using namespace BaseSimulator::utils;

TiXmlNode *Target::targetListNode = NULL;
TiXmlNode *Target::targetNode = NULL;

Target *Target::loadNextTarget() {
    if (Target::targetListNode) {
        // Move targetNode pointer to next target (or NULL if there is none)
        Target::targetNode = targetListNode->IterateChildren(targetNode); 

        if (Target::targetNode) {
            TiXmlElement* element;
            if (Target::targetNode) {
                element = Target::targetNode->ToElement();
                const char *attr = element->Attribute("format");
                if (attr) {
                    string str(attr);
                    if (str.compare("grid") == 0) {
                        return new TargetGrid(Target::targetNode);
                    } else if (str.compare("csg") == 0) {
                        return new TargetCSG(Target::targetNode);
                    } else if (str.compare("surface") == 0) {
                        return new TargetSurface(Target::targetNode);
                    }
                }
            }
        }
    }

    return NULL;
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
            cerr << "error: position attribute missing for target cell" << endl;
            throw TargetParsingException();
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

bool TargetGrid::isInTarget(const Cell3DPosition &pos) {
    return tCells.count(pos);
}

const Color TargetGrid::getTargetColor(const Cell3DPosition &pos) {
    if (!isInTarget(pos)) {
        cerr << "error: attempting to get color of undefined target cell" << endl;
        throw InvalidPositionException();
    }

    return tCells[pos];
}

void TargetGrid::addTargetCell(const Cell3DPosition &pos, const Color c) {
    tCells.insert(std::pair<const Cell3DPosition, const Color>(pos, c));
}

void TargetGrid::print(ostream& where) const {
    for(auto const& pair : tCells) {
        where << "<cell position=" << pair.first << " color=" << pair.second << " />" << endl;
    }
}

void TargetGrid::boundingBox(BoundingBox &bb) {
    throw BaseSimulator::utils::NotImplementedException();
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

    char* csgBin = CSGParser::parseCsg(str);
    CsgUtils csgUtils;
    csgRoot = csgUtils.readCSGBuffer(csgBin);
    csgRoot->toString();
    if (boundingBox)
        csgRoot->boundingBox(bb);
}

Vector3D TargetCSG::gridToWorldPosition(const Cell3DPosition &pos) {
    Vector3D worldPosition;
    worldPosition.pt[3] = 1.0;
    worldPosition.pt[2] = M_SQRT2_2 * (pos[2] + 0.5);
    if (IS_EVEN(pos[2])) {
        worldPosition.pt[1] = (pos[1] + 0.5);
        worldPosition.pt[0] = (pos[0] + 0.5);
    } else {
        worldPosition.pt[1] = (pos[1] + 1.0);
        worldPosition.pt[0] = (pos[0] + 1.0);
    }
    worldPosition.pt[0] += bb.P0[0];
    worldPosition.pt[1] += bb.P0[1];
    worldPosition.pt[2] += bb.P0[2];
    return worldPosition;
}

bool TargetCSG::isInTarget(const Cell3DPosition &pos) {
    Color color;
    return csgRoot->isInside(gridToWorldPosition(pos), color);
}

bool TargetCSG::isInTargetBorder(const Cell3DPosition &pos, double radius) {
    Color color;
    return csgRoot->isInBorder(gridToWorldPosition(pos), color, radius);
}

void TargetCSG::boundingBox(BoundingBox &bb) {
    csgRoot->boundingBox(bb);
}

const Color TargetCSG::getTargetColor(const Cell3DPosition &pos) {
    Color color;
    if (!csgRoot->isInside(gridToWorldPosition(pos), color)) {
        cerr << "error: attempting to get color of undefined target cell" << endl;
        throw InvalidPositionException();
    }
    return color;
}

/************************************************************
 *                      TargetSurface
 ************************************************************/

TargetSurface::TargetSurface(TiXmlNode *targetNode) : Target(targetNode) {    
    TiXmlNode *methNode = targetNode->FirstChild("method");
    const char* attr;
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
                else if (str.compare("voisin") == 0) {
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
                    cerr << "error: position attribute missing for target cell" << endl;
                    throw TargetParsingException();
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
        while (pcl.size() > (d+1)*(d+2)/2){
            d = d+1;
        }
        if (pcl.size() < (d+1)*(d+2)/2){
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
}

bool TargetSurface::isInTarget(const Cell3DPosition &pos) {
    //Initialization
    Vector3D cartesianpos = getWorld()->lattice->gridToWorldPosition(pos);
    float x = cartesianpos.pt[0];
    float y = cartesianpos.pt[1];
    float z = cartesianpos.pt[2];

    if (method.compare("interpolation") == 0) {
        //Calculation of d polynom degree
        int d = 0;
        while (coeffs.size()>(d+1)*(d+2)/2) {
            d = d+1;
        }
        if (coeffs.size()<(d+1)*(d+2)/2) {
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
        cout << "x=" << x << ",y=" << y << ",z=" << z <<  " f= " << f << endl;
        if (z<=f){
            //cout << "x=" << x << ",y=" << y << " f= " << f << endl;
            return true;
        }
        else {
            return false;
        }
    }

    else if (method.compare("voisin") == 0) {
        //print method
        cout << "Call to isInTarget method =" << method << endl; 
        //Nearest neighboor research
        float mindist = FLT_MAX;
        int neighbor = 0;
        for (int i=0;i<pcl.size();i++){
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
    
    else {
        throw BaseSimulator::utils::NotImplementedException();
    }
}

const Color TargetSurface::getTargetColor(const Cell3DPosition &pos) {
    throw BaseSimulator::utils::NotImplementedException();
}

void TargetSurface::addTargetCell(const Cell3DPosition &pos, const Color c) {
    Vector3D v;
    v.set(pos.pt[0],pos.pt[1],pos.pt[2],1);
    pcl.push_back(v);
    cout << "Point ("<<v.pt[0]<<","<<v.pt[1]<<","<<v.pt[2]<<") added to pointCloud" << endl;
}

void TargetSurface::print(ostream& where) const {

    if (method.compare("interpolation") == 0) {
        //print method
        cout << "Call to print method =" << method << endl; 
    }

    else if (method.compare("voisin") == 0) {
        //print method
        cout << "Call to print method =" << method << endl; 
    }
    
    else {
        throw BaseSimulator::utils::NotImplementedException();
    }
}

void TargetSurface::boundingBox(BoundingBox &bb) {
    throw BaseSimulator::utils::NotImplementedException();
}

} // namespace BaseSimulator
