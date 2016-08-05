/*! @file target.cpp
 * @brief Defines a target configuration for reconfiguration algorithms, 
 * several ways of defining the configuration are provided to the user.
 * @author Pierre Thalamy
 * @date 21/07/2016
 */

#include "target.h"
#include "utils.h"

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
                        throw NotImplementedException();
                        return new TargetCSG(Target::targetNode);
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

/************************************************************
 *                      TargetCSG
 ************************************************************/


bool TargetCSG::isInTarget(const Cell3DPosition &pos) {
    throw utils::NotImplementedException();
}

const Color TargetCSG::getTargetColor(const Cell3DPosition &pos) {
    throw utils::NotImplementedException();
}

} // namespace BaseSimulator
