/*
 * catoms3DSimulator.cpp
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#include <iostream>
#include "catoms3DSimulator.h"
#include <string.h>
#include "trace.h"
#include "utils.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace Catoms3D {

void Catoms3DSimulator::help() {
	cerr << "VisibleSim:" << endl;
	cerr << "Catoms3D" << endl;
	exit(EXIT_SUCCESS);
}

Catoms3DSimulator::Catoms3DSimulator(int argc, char *argv[], Catoms3DBlockCode *(*bcb)(Catoms3DBlock*))
	: BaseSimulator::Simulator(argc, argv, (BlockCodeBuilder)bcb) {
	OUTPUT << "\033[1;34m" << "Catoms3DSimulator constructor" << "\033[0m" << endl;
}

Catoms3DSimulator::~Catoms3DSimulator() {
	OUTPUT << "\033[1;34m" << "Catoms3DSimulator destructor" << "\033[0m" <<endl;
}

void Catoms3DSimulator::createSimulator(int argc, char *argv[], Catoms3DBlockCode *(*catoms3DBlockCodeBuildingFunction)(Catoms3DBlock*)) {
	simulator =  new Catoms3DSimulator(argc, argv, catoms3DBlockCodeBuildingFunction);
	simulator->parseConfiguration(argc, argv);
	simulator->startSimulation();
}

void Catoms3DSimulator::loadWorld(const Cell3DPosition &gridSize, const Vector3D &gridScale,
				      int argc, char *argv[]) {
    world = new Catoms3DWorld(gridSize, gridScale, argc, argv);
    world->loadTextures("../../simulatorCore/catoms3DTextures");
    World::setWorld(world);
}

void Catoms3DSimulator::loadBlock(TiXmlElement *blockElt, int blockId, BlockCodeBuilder bcb,
								  const Cell3DPosition &pos, const Color &color, bool master) {

	// Any additional configuration file parsing exclusive to this type of block should be performed
	//  here, using the blockElt TiXmlElement.

	// set the orientation
	int orientation = 0;
	const char *attr = blockElt->Attribute("orientation");
	if (attr) {
		orientation = atoi(attr);
		OUTPUT << "orientation : " << orientation << endl;
	}

	// Finally, add block to the world
	((Catoms3DWorld*)world)->addBlock(blockId, bcb, pos, color, orientation, master);
}

// PTHY: TODO: Refactor / Genericize
void Catoms3DSimulator::parseSkeleton() {
/* loading skeleton*/
	TiXmlNode *nodeGrid = xmlWorldNode->FirstChild("skeleton");
	if (nodeGrid) {
		Skeleton *sk = new Skeleton();
		((Catoms3DWorld*)world)->setSkeleton(sk);
		/* skeleton points */
		TiXmlNode *skelPoint = nodeGrid->FirstChild("skeleton_point");
		Vector3D position;
		double radius=10,blobbiness=-0.1;
		const char *attr;
		TiXmlElement* element;
		while (skelPoint) {
			element = skelPoint->ToElement();
			attr = element->Attribute("pos");
			if (attr) {
				string str(attr);
				int pos1 = str.find_first_of(','),
					pos2 = str.find_last_of(',');
				position.pt[0] = atof(str.substr(0,pos1).c_str());
				position.pt[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str());
				position.pt[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str());
				OUTPUT << "point position : " << position << endl;
			}
			attr = element->Attribute("radius");
			if (attr) {
				radius = atof(attr);
				OUTPUT << "radius : " << radius << endl;
			}
			attr = element->Attribute("blobbiness");
			if (attr) {
				blobbiness = atof(attr);
				OUTPUT << "blobbiness: " << blobbiness << endl;
			}
			sk->add(new SkelPoint(position,radius,blobbiness));
			skelPoint = skelPoint->NextSibling("skeleton_point");
		}
		/* skeleton points */
		TiXmlNode *skelLine = nodeGrid->FirstChild("skeleton_line");
		Vector3D posA,posB;
		while (skelLine) {
			element = skelLine->ToElement();
			attr = element->Attribute("posA");
			if (attr) {
				string str(attr);
				int pos1 = str.find_first_of(','),
					pos2 = str.find_last_of(',');
				posA.pt[0] = atof(str.substr(0,pos1).c_str());
				posA.pt[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str());
				posA.pt[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str());
				OUTPUT << "point position : " << posA << endl;
			}
			attr = element->Attribute("posB");
			if (attr) {
				string str(attr);
				int pos1 = str.find_first_of(','),
					pos2 = str.find_last_of(',');
				posB.pt[0] = atof(str.substr(0,pos1).c_str());
				posB.pt[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str());
				posB.pt[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str());
				OUTPUT << "point position : " << posB << endl;
			}
			attr = element->Attribute("radius");
			if (attr) {
				radius = atof(attr);
				OUTPUT << "radius : " << radius << endl;
			}
			attr = element->Attribute("blobbiness");
			if (attr) {
				blobbiness = atof(attr);
				OUTPUT << "blobbiness: " << blobbiness << endl;
			}
			sk->add(new SkelLine(posA,posB,radius,blobbiness));
			skelLine = skelLine->NextSibling("skeleton_point");
		}
	} else {
		ERRPUT << "No skeleton" << endl;
	}
}

} // Catoms3D namespace
