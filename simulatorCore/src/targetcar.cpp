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
    throw BaseSimulator::NotImplementedException("TargetGrid::boundingBox");
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

    if (method.compare("nurbs") == 0) {
        cout << "Call to the NURBS surface type initialization" << endl;
        //Initialization of the NURBS parameters, parsing from config file to be implemented later
        S_NUMPOINTS = 7;
        S_ORDER = 3;
        S_NUMKNOTS = S_NUMPOINTS + S_ORDER;
        T_NUMPOINTS = 4;
        T_ORDER = 3;
        T_NUMKNOTS = T_NUMPOINTS + T_ORDER;
        
        //Filling sknots
        sknots.push_back(0);
        sknots.push_back(0.1);
        sknots.push_back(0.2);
        sknots.push_back(0.3);
        sknots.push_back(0.4);
        sknots.push_back(0.5);
        sknots.push_back(0.6);
        sknots.push_back(0.7);
        sknots.push_back(0.8);
        sknots.push_back(1);

        //Checking if sknots correctly filled

        for (int i=0; i < S_NUMKNOTS; i++){
            cout << "sknots[" << i << "] = " << sknots[i] << endl;
        }

        //Filling tknots
        tknots.push_back(0);
        tknots.push_back(0.15);
        tknots.push_back(0.3);
        tknots.push_back(0.45);
        tknots.push_back(0.60);
        tknots.push_back(0.75);
        tknots.push_back(1);

        //Checking if tknots correctly filled

        for (int i=0; i < T_NUMKNOTS; i++){
            cout << "tknots[" << i << "] = " << tknots[i] << endl;
        }

        //Filling ctlpoints
        for (int i=0; i < S_NUMPOINTS; i++){
            vector<vector<float>> tctlpoint;
            for (int j=0; j < T_NUMPOINTS; j++){
                vector<float> point;
                float l=0;
                float k=-33.33;
                if (i==1) {
                    l = 6.66;
                    k = 40;
                    if (j==1) {
                        k = k+13.33;
                    }
                    if (j==2) {
                        k = k+13.33;
                    }
                }
                if (i==2) {
                    l = 66.66;
                    k = 66.66;
                    if (j==1) {
                        k = k+13.34;
                    }
                    if (j==2) {
                        k = k+13.34;
                    }
                }
                if (i==3) {
                    l = 116.66;
                    k = 66.66;
                    if (j==1) {
                        k = k+13.34;
                    }
                    if (j==2) {
                        k = k+13.34;
                    }
                }
                if (i==4) {
                    l = 150;
                    k = 26.66;
                    if (j==1) {
                        k = k+13.34;
                    }
                    if (j==2) {
                        k = k+13.34;
                    }
                }
                if (i==5) {
                    l = 193.33;
                    k = 26.66;
                    if (j==1) {
                        k = k+13.34;
                    }
                    if (j==2) {
                        k = k+13.34;
                    }
                }
                if (i==6) {
                    l = 200;
                    k = -20;
                }
                point.push_back(l);
                point.push_back(j*33.33);
                point.push_back(k);
                point.push_back(33.33);
                tctlpoint.push_back(point);
                point.clear();
             }
             ctlpoints.push_back(tctlpoint);
             tctlpoint.clear();
        }

        //ctlpoints[2][1][2]=270;
        //ctlpoints[3][3][2]=90;

        //Checking ctlpoints
        
        for (int i=0; i < S_NUMPOINTS; i++){
            for (int j=0; j < T_NUMPOINTS; j++){
                cout << "ctlpoints["<<i<<"]["<<j<<"][2] = "<< ctlpoints[i][j][2]<< endl;
             }
        }
        //NURBS parameters initialization finished
    }

}

bool TargetSurface::isInTarget(const Cell3DPosition &pos) {
    //Initialization
    Vector3D cartesianpos = getWorld()->lattice->gridToWorldPosition(pos);
    float x = cartesianpos.pt[0];
    float y = cartesianpos.pt[1];
    float z = cartesianpos.pt[2];
    float xfound = 0;
    float yfound = 0;

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


    else if (method.compare("nurbs") == 0) {
        //print method
        //cout << "Call to isInTarget method =" << method << endl; 
        //Initialization of dichotomy parameters
        float precision = FLT_MAX;
        float precGoal = 100;
        float approx = 0.01;
        float u0 = 0 + approx;
        float u1 = 1 - approx;
        float v0 = 0 + approx;
        float v1 = 1 - approx;
        float znurbs = 0;
        int count = 0;

        //Begin dichotomy process
        while (count<10){
        // || precision>precGoal || ((u0 == u1) && (v0 == v1))){
            float mindist = FLT_MAX;
            int quadrant = 0;
            float x1 = calculateNurbs(u0,v0,0);
            float y1 = calculateNurbs(u0,v0,1);
            float x2 = calculateNurbs(u1,v0,0);
            float y2 = calculateNurbs(u1,v0,1);
            float x3 = calculateNurbs(u0,v1,0);
            float y3 = calculateNurbs(u0,v1,1);
            float x4 = calculateNurbs(u1,v1,0);
            float y4 = calculateNurbs(u1,v1,1);
            if (count == 0){
                //cout << "x1=" <<x1<<",x2="<<x2<<",x3="<<x3<<",x4="<<x4<<endl;
                //cout << "y1=" <<y1<<",y2="<<y2<<",y3="<<y3<<",y4="<<y4<<endl;
                if ( x1 < x && x2 < x && x3 < x && x4 < x ){
                    return false;
                }
                if ( y1 < y && y2 < y && y3 < y && y4 < y ){
                    return false;
                }
            }
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
                xfound = x1;
                yfound = y1;
                u1 = (u1+u0)/2;
                v1 = (v1+v0)/2;
            }
            if (quadrant == 2){
//                cout << "my x="<<x<<" my y="<<y<<" x found="<<x2<<" y found="<<y2<<endl;
                xfound = x2;
                yfound = y2;
                znurbs = calculateNurbs(u1,v0,2);
                u0 = (u1+u0)/2;
                v1 = (v1+v0)/2;
            }
            if (quadrant == 3){
//                cout << "my x="<<x<<" my y="<<y<<" x found="<<x3<<" y found="<<y3<<endl;
                xfound = x3;
                yfound = y3;
                znurbs = calculateNurbs(u0,v1,2);
                u1 = (u1+u0)/2;
                v0 = (v1+v0)/2;
            }
            if (quadrant == 4){
//                cout << "my x="<<x<<" my y="<<y<<" x found="<<x4<<" y found="<<y4<<endl;
                xfound = x4;
                yfound = y4;
                znurbs = calculateNurbs(u1,v1,2);
                u0 = (u1+u0)/2;
                v0 = (v1+v0)/2;
            }
            
            precision = sqrt(mindist);
            //cout<<"quadrant="<<quadrant<<endl;
            //cout<<"x="<<x<<",x1="<<x1<<",x2="<<x2<<",x3="<<x3<<",x4="<<x4<<endl;
            //cout<<"y="<<y<<",y1="<<y1<<",y2="<<y2<<",y3="<<y3<<",y4="<<y4<<endl;
            count += 1;
        }
        cout << "x=" << x << ", y=" << y << ", xfound=" << xfound << ", yfound=" << yfound <<endl;
        cout << "End of while: znurbs=" << znurbs << " ,précision =" << precision << endl;
        //Comparison between z
        if (z<=znurbs){
            return true;
        }
        else{
            return false;
        }        
    }
    
    else {
        throw BaseSimulator::NotImplementedException("TargetSurface::isInTarget");
    }
}

const Color TargetSurface::getTargetColor(const Cell3DPosition &pos) {
    throw BaseSimulator::utils::NotImplementedException("TargetSurface::getTargetColor");
}

void TargetSurface::addTargetCell(const Cell3DPosition &pos, const Color c) {
    Vector3D v;
    v.set(pos.pt[0],pos.pt[1],pos.pt[2],1);
    pcl.push_back(v);
    cout << "Point ("<<v.pt[0]<<","<<v.pt[1]<<","<<v.pt[2]<<") added to pointCloud" << endl;
}


float TargetSurface::calculateNurbs(float u, float v, int coord){

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
    for (int i =0; i < S_NUMPOINTS; i++){
        for (int j=0; j < T_NUMPOINTS; j++){
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

float TargetSurface::dist(float x1, float y1, float x2, float y2){
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

void TargetSurface::boundingBox(BoundingBox &bb) {
    throw BaseSimulator::utils::NotImplementedException("TargetSurface::boundingBox");
}

} // namespace BaseSimulator
