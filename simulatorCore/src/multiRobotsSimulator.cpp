/*
 * @file multiRobotsSimulator.cpp
 *
 *  Created on: 14/07/16
 *      Author: pthalamy
 */

#include <iostream>
#include <string.h>

#include "multiRobotsSimulator.h"
#include "trace.h"

using namespace std;

namespace MultiRobots {

MultiRobotsSimulator::MultiRobotsSimulator(int argc, char *argv[],
                                           MultiRobotsBlockCode *(*bcb)(MultiRobotsBlock*))
	: BaseSimulator::Simulator(argc, argv, (BlockCodeBuilder)bcb) {
	OUTPUT << "\033[1;34m" << "MultiRobotsSimulator constructor" << "\033[0m" << endl;
}

MultiRobotsSimulator::~MultiRobotsSimulator() {
	OUTPUT << "\033[1;34m" << "MultiRobotsSimulator destructor" << "\033[0m" <<endl;
}

void MultiRobotsSimulator::createSimulator(int argc, char *argv[],
                                           MultiRobotsBlockCode *(*bcb)(MultiRobotsBlock*)) {
	simulator =  new MultiRobotsSimulator(argc, argv, bcb);
	simulator->parseConfiguration(argc, argv);
	simulator->startSimulation();
}

void MultiRobotsSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
                                     int argc, char *argv[]) {
	world = new MultiRobotsWorld(gridSize, gridScale, argc,argv);

	if (GlutContext::GUIisEnabled) // PTHY: INCONSISTENCY
		world->loadTextures("../../simulatorCore/multiRobotsTextures");

	World::setWorld(world);
}

void MultiRobotsSimulator::loadBlock(TiXmlElement *blockElt, int blockId, BlockCodeBuilder bcb,
									  const Cell3DPosition &pos, const Color &color, bool master) {

	// Any additional configuration file parsing exclusive to this type of block should be performed
	//  here, using the blockElt TiXmlElement.

	// ...Parsing code...

	// Finally, add block to the world
	((MultiRobotsWorld*)world)->addBlock(blockId, bcb, pos, color, 0, master);
}

// OBSTACLES
// // loading the obstacles
// TiXmlNode *nodeObstacle = node->FirstChild("obstacleList");
// if (nodeObstacle) {
//     Color defaultColor = DARKGREY;
//     TiXmlElement* element = nodeObstacle->ToElement();
//     const char *attr= element->Attribute("color");
//     if (attr) {
//         string str(attr);
//         int pos1 = str.find_first_of(','),
// 			pos2 = str.find_last_of(',');
//         defaultColor.rgba[0] = atof(str.substr(0,pos1).c_str())/255.0;
//         defaultColor.rgba[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str())/255.0;
//         defaultColor.rgba[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str())/255.0;
//     }

//     nodeObstacle = nodeObstacle->FirstChild("obstacle");
//     Vecteur position;
//     Color color;
//     while (nodeObstacle) {
//         element = nodeObstacle->ToElement();
//         color=defaultColor;
//         attr = element->Attribute("color");
//         if (attr) {
//             string str(attr);
//             int pos1 = str.find_first_of(','),
// 				pos2 = str.find_last_of(',');
//             color.set(atof(str.substr(0,pos1).c_str())/255.0,
//                       atof(str.substr(pos1+1,pos2-pos1-1).c_str())/255.0,
//                       atof(str.substr(pos2+1,str.length()-pos1-1).c_str())/255.0);
//             OUTPUT << "color :" << color << endl;
//         }
//         attr = element->Attribute("position");
//         if (attr) {
//             string str(attr);
//             int pos1 = str.find_first_of(','),
// 				pos2 = str.find_last_of(',');
//             position.pt[0] = atoi(str.substr(0,pos1).c_str());
//             position.pt[1] = atoi(str.substr(pos1+1,pos2-pos1-1).c_str());
//             position.pt[2] = atoi(str.substr(pos2+1,str.length()-pos1-1).c_str());
//             OUTPUT << "position : " << position << endl;
//         }
//         world->addObstacle(position, color);
//         nodeObstacle = nodeObstacle->NextSibling("obstacle");
//     } // end while (nodeObstacle)
// } else { // end if(nodeObstacle)
//     ERRPUT << "no Block List" << endl;
// }

} // MultiRobots namespace
