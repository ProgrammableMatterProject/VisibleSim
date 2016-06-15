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

Catoms3DBlockCode*(* Catoms3DSimulator::buildNewBlockCode)(Catoms3DBlock*)=NULL;

void Catoms3DSimulator::help() {
   cerr << "VisibleSim:" << endl;
	cerr << "Robot01" << endl;
   exit(EXIT_SUCCESS);
}

Catoms3DSimulator::Catoms3DSimulator(int argc, char *argv[], Catoms3DBlockCode *(*catoms3DBlockCodeBuildingFunction)(Catoms3DBlock*)) : BaseSimulator::Simulator(argc, argv) {
//	OUTPUT << "\033[1;34m" << "Catoms3DSimulator constructor" << "\033[0m" << endl;

	int currentID = 1;
	Catoms3DWorld *world = NULL;
	buildNewBlockCode = catoms3DBlockCodeBuildingFunction;
	float blockSize[3];

	testMode = false;

	/* reading the xml file */
	TiXmlNode *node = xmlDoc->FirstChild("world");
	if (node) {
		TiXmlElement* worldElement = node->ToElement();
		const char *attr= worldElement->Attribute("gridSize");
		int lx = 0;
                int ly = 0;
                int lz = 0;
		if (attr) {
			string str=attr;
			int pos1 = str.find_first_of(','),
				pos2 = str.find_last_of(',');
			lx = atoi(str.substr(0,pos1).c_str());
			ly = atoi(str.substr(pos1+1,pos2-pos1-1).c_str());
			lz = atoi(str.substr(pos2+1,str.length()-pos1-1).c_str());
			OUTPUT << "grid size : " << lx << " x " << ly << " x " << lz << endl;
		} else {
			OUTPUT << "WARNING No grid size in XML file" << endl;
		}
		attr=worldElement->Attribute("windowSize");
		if (attr) {
			string str=attr;
	 		int pos = str.find_first_of(',');
			GlutContext::initialScreenWidth = atoi(str.substr(0,pos).c_str());
			GlutContext::initialScreenHeight = atoi(str.substr(pos+1,str.length()-pos-1).c_str());
			GlutContext::screenWidth = GlutContext::initialScreenWidth;
			GlutContext::screenHeight = GlutContext::initialScreenHeight;
		}

		createWorld(lx, ly, lz, argc, argv);
		world = getWorld();
		world->loadTextures("../../simulatorCore/catoms3DTextures");

	} else {
		ERRPUT << "ERROR : NO world in XML file" << endl;
		exit(1);
	}

	createScheduler();

	// loading the camera parameters
	TiXmlNode *nodeConfig = node->FirstChild("camera");
	if (nodeConfig) {
		TiXmlElement* cameraElement = nodeConfig->ToElement();
		const char *attr=cameraElement->Attribute("target");
		double def_near=0.1,def_far=1500;
		float angle=45.0;
		if (attr) {
			string str(attr);
			int pos1 = str.find_first_of(','),
			pos2 = str.find_last_of(',');
			Vector3D target;
			target.pt[0] = atof(str.substr(0,pos1).c_str());
			target.pt[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str());
			target.pt[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str());
			world->getCamera()->setTarget(target);
		}
		attr=cameraElement->Attribute("angle");
		if (attr) {
			angle = atof(attr);
			world->getCamera()->setAngle(angle);
		}
		attr=cameraElement->Attribute("directionSpherical");
		if (attr) {
			string str(attr);
			int pos1 = str.find_first_of(','),
			pos2 = str.find_last_of(',');
			float az,ele,dist;
			az = -90.0+atof(str.substr(0,pos1).c_str());
			ele = atof(str.substr(pos1+1,pos2-pos1-1).c_str());
			dist = atof(str.substr(pos2+1,str.length()-pos1-1).c_str());
			world->getCamera()->setDirection(az,ele);
			world->getCamera()->setDistance(dist);
			az = dist*sin(angle*M_PI/180.0);
			def_near = dist-az;
			def_far = dist+az;
		}
		attr=cameraElement->Attribute("near");
		if (attr) {
			def_near = atof(attr);
		}
		attr=cameraElement->Attribute("far");
		if (attr) {
			def_far = atof(attr);
		}
		world->getCamera()->setNearFar(def_near,def_far);
	}

	// loading the spotlight parameters
	nodeConfig = node->FirstChild("spotlight");
	if (nodeConfig) {
		Vector3D target;
		float az=0,ele=60,dist=1000,angle=50;
		double def_near=10.0,def_far=1500.0;
		TiXmlElement* lightElement = nodeConfig->ToElement();
		const char *attr=lightElement->Attribute("target");
		if (attr) {
			string str(attr);
			int pos1 = str.find_first_of(','),
			pos2 = str.find_last_of(',');
			target.pt[0] = atof(str.substr(0,pos1).c_str());
			target.pt[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str());
			target.pt[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str());
		}
		attr=lightElement->Attribute("directionSpherical");
		if (attr) {
			string str(attr);
			int pos1 = str.find_first_of(','),
			pos2 = str.find_last_of(',');
			az = -90.0+atof(str.substr(0,pos1).c_str());
			ele = atof(str.substr(pos1+1,pos2-pos1-1).c_str());
			dist = atof(str.substr(pos2+1,str.length()-pos1-1).c_str());
		}
		attr=lightElement->Attribute("angle");
		if (attr) {
			angle = atof(attr);
		}
		attr=lightElement->Attribute("near");
		if (attr) {
			def_near = atof(attr);
		}
		attr=lightElement->Attribute("far");
		if (attr) {
			def_far = atof(attr);
		}
		world->getCamera()->setLightParameters(target,az,ele,dist,angle,def_near,def_far);

	}

	TiXmlNode *nodeBlock = node->FirstChild("blockList");
	if (nodeBlock) {
		Color defaultColor=DARKGREY;
		TiXmlElement* element = nodeBlock->ToElement();
		const char *attr= element->Attribute("color");
		if (attr) {
			string str(attr);
			int pos1 = str.find_first_of(','),
			pos2 = str.find_last_of(',');
			defaultColor.rgba[0] = atof(str.substr(0,pos1).c_str())/255.0;
			defaultColor.rgba[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str())/255.0;
			defaultColor.rgba[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str())/255.0;
			OUTPUT << "new default color :" << defaultColor << endl;
		}
		attr= element->Attribute("blocksize");
        if (attr) {
             string str(attr);
             int pos1 = str.find_first_of(','),
              pos2 = str.find_last_of(',');
             blockSize[0] = atof(str.substr(0,pos1).c_str());
             blockSize[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str());
             blockSize[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str());
             OUTPUT << "blocksize =" << blockSize[0] << "," << blockSize[1] << "," << blockSize[2] << endl;
             world->setBlocksSize(blockSize);
		}

	/* Reading a block */
		TiXmlNode *block = nodeBlock->FirstChild("block");
		Cell3DPosition position;
		int orientation=0;
		Color color;
		bool master;
		while (block) {
		   element = block->ToElement();
		   color=defaultColor;
		   master=false;
		   orientation=0;
		   // set the color
		   attr = element->Attribute("color");
		   if (attr) {
			   string str(attr);
			   int pos1 = str.find_first_of(','),
		   		   pos2 = str.find_last_of(',');
			   color.set(atof(str.substr(0,pos1).c_str())/255.0,
                         atof(str.substr(pos1+1,pos2-pos1-1).c_str())/255.0,
                         atof(str.substr(pos2+1,str.length()-pos1-1).c_str())/255.0);
			   OUTPUT << "new color :" << defaultColor << endl;
			}
			// set the position
			attr = element->Attribute("position");
			if (attr) {
                string str(attr);
                int pos1 = str.find_first_of(','),
                    pos2 = str.find_last_of(',');
				position.set(atof(str.substr(0,pos1).c_str()),
                             atof(str.substr(pos1+1,pos2-pos1-1).c_str()),
                             atof(str.substr(pos2+1,str.length()-pos1-1).c_str()));
				OUTPUT << "position : " << position << endl;
			}
			// set the orientation
			attr = element->Attribute("orientation");
			if (attr) {
                orientation = atoi(attr);
                OUTPUT << "orientation : " << orientation << endl;
			}
			attr = element->Attribute("master");
			if (attr) {
                string str(attr);
                if (str.compare("true")==0 || str.compare("1")==0) {
                    master=true;
                }
                OUTPUT << "master : " << master << endl;
			}
			world->addBlock(currentID++,Catoms3DSimulator::buildNewBlockCode,position,orientation,color,master);
			block = block->NextSibling("block");
		} // end while (block)
/*
		block = nodeBlock->FirstChild("blocksLine");
		int line,plane;
		while (block) {
			element = block->ToElement();
			color=defaultColor;
			attr = element->Attribute("color");
			if (attr) {
				string str(attr);
				int pos1 = str.find_first_of(','),
					pos2 = str.find_last_of(',');
				color.rgba[0] = atof(str.substr(0,pos1).c_str())/255.0;
				color.rgba[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str())/255.0;
				color.rgba[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str())/255.0;
				OUTPUT << "line color :" << color << endl;

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
				position.pt[2] = plane;
				position.pt[1] = line;
				int n = str.length();
				for(int i=0; i<n; i++) {
			    	if  (str[i]=='1') {
			    		position.pt[0]=i;
			    		world->addBlock(currentID++,Catoms3DSimulator::buildNewBlockCode,position,color);
			    	}
			    }
			}
			block = block->NextSibling("blocksLine");
		} // end while (nodeBlock)*/
	} else // end if(nodeBlock)
	{ cerr << "no Block List" << endl;
	}
/* loading skeleton*/
    TiXmlNode *nodeGrid = node->FirstChild("skeleton");
	if (nodeGrid) {
		Skeleton *sk = new Skeleton();
		world->setSkeleton(sk);
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
/*
	TiXmlNode *nodeGrid = node->FirstChild("targetGrid");
	if (nodeGrid) {
		world->initTargetGrid();
		TiXmlNode *block = nodeGrid->FirstChild("block");
		Vector3D position;
		const char *attr;
		TiXmlElement* element;
		while (block) {
		   	element = block->ToElement();
			attr = element->Attribute("position");
			if (attr) {
                string str(attr);
                int pos = str.find_first_of(',');
                int ix = atof(str.substr(0,pos).c_str()),
                    iy = atoi(str.substr(pos+1,str.length()-pos-1).c_str());
				position.pt[0] = ix;
				position.pt[1] = 0;
				position.pt[2] = iy;
			}
			world->setTargetGrid(fullCell,position[0],position[1],position[2]);
			block = block->NextSibling("block");
		}

		TiXmlNode *block = nodeGrid->FirstChild("targetLine");
		int line,plane;
		while (block) {
			TiXmlElement* element = block->ToElement();
			const char *attr = element->Attribute("line");
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
	 			int n = str.length();
	 			for(int i=0; i<n; i++) {
	 		    	world->setTargetGrid((str[i]=='1')?fullCell:emptyCell,i,line,plane);
	 		    }
	 		}
	 		block = block->NextSibling("targetLine");
	 	}
	 } else {
	 	ERRPUT << "No target grid" << endl;
	 }*/
/*
    TiXmlNode *nodeCapa = node->FirstChild("capabilities");
	if (nodeCapa) {
        world->setCapabilities(new Catoms3DCapabilities(nodeCapa));
    }
*/
    world->linkBlocks();

//	getScheduler()->sem_schedulerStart->post();
//	getScheduler()->setState(Scheduler::NOTSTARTED);

	if (!testMode) {
      GlutContext::mainLoop();
   }
}

Catoms3DSimulator::~Catoms3DSimulator() {
	OUTPUT << "\033[1;34m" << "Catoms3DSimulator destructor" << "\033[0m" <<endl;
}

void Catoms3DSimulator::createSimulator(int argc, char *argv[], Catoms3DBlockCode *(*catoms3DBlockCodeBuildingFunction)(Catoms3DBlock*)) {
	simulator =  new Catoms3DSimulator(argc, argv, catoms3DBlockCodeBuildingFunction);
}

void Catoms3DSimulator::deleteSimulator() {
	delete((Catoms3DSimulator*)simulator);
}

} // Catoms3D namespace
