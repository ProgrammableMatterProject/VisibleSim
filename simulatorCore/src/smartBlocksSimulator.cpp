/*
 * smartBlocksSimulator.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include "smartBlocksSimulator.h"

using namespace std;

namespace SmartBlocks {

SmartBlocksBlockCode*(* SmartBlocksSimulator::buildNewBlockCode)(SmartBlocksBlock*)=NULL;

SmartBlocksSimulator::SmartBlocksSimulator(int argc, char *argv[],
					   SmartBlocksBlockCode *(*smartBlocksBlockCodeBuildingFunction)
					   (SmartBlocksBlock*)) : BaseSimulator::Simulator(argc, argv) {
	cout << "\033[1;34m" << "SmartBlocksSimulator constructor" << "\033[0m" << endl;

	buildNewBlockCode = smartBlocksBlockCodeBuildingFunction;
	createScheduler();

	int currentID=1;
	SmartBlocksWorld *world=NULL;
/* reading the xml file */
	TiXmlNode *node = xmlDoc->FirstChild("world");
	if (node) {
		TiXmlElement* worldElement = node->ToElement();
		string str = worldElement->Attribute("gridSize");
		int pos = str.find_first_of(',');
		int largeur = atoi(str.substr(0,pos).c_str());
		int hauteur = atoi(str.substr(pos+1,str.length()-pos-1).c_str());
		cout << "grid size : " << largeur << " x " << hauteur << endl;
		const char *attr=worldElement->Attribute("maxSimulationTime");
		if (attr) {
			str=attr;
			uint64_t t = atoi(attr);
			int l = strlen(attr);
			if (str.substr(l-2,2)=="mn") {
				t*=60000000;
			} else if (str.substr(l-2,2)=="ms") {
				t*=1000;
			} else if (str.substr(l-1,1)=="s") {
				t*=1000000;
			}
			getScheduler()->setMaximumDate(t);
		}
		attr=worldElement->Attribute("windowSize");
		if (attr) {
			str=attr;
			int pos = str.find_first_of(',');
			GlutContext::initialScreenWidth = atoi(str.substr(0,pos).c_str());
			GlutContext::initialScreenHeight = atoi(str.substr(pos+1,str.length()-pos-1).c_str());
			GlutContext::screenWidth = GlutContext::initialScreenWidth;
			GlutContext::screenHeight = GlutContext::initialScreenHeight;
		}
		createWorld(largeur,hauteur,argc,argv);
		world = getWorld();
		world->loadTextures("../../simulatorCore/smartBlocksTextures");
	} else {
		cerr << "ERROR : NO world in XML file" << endl;
		exit(1);
	}
	TiXmlNode *nodeConfig = node->FirstChild("camera");
	if (nodeConfig) {
		TiXmlElement* cameraElement = nodeConfig->ToElement();
		const char *attr=cameraElement->Attribute("target");
		double def_near=1,def_far=1500;
		float angle=45;
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
		float farplane=3.0*dist*tan(angle*M_PI/180.0);
		world->getCamera()->setLightParameters(target,az,ele,dist,angle,10.0,farplane);
	}

    TiXmlNode *nodeBlock = node->FirstChild("blockList");
	if (nodeBlock) {
		Color defaultColor(0.8,0.8,0.8);
		TiXmlElement* element = nodeBlock->ToElement();
		const char *attr= element->Attribute("color");
		if (attr) {
			string str(attr);
			int pos1 = str.find_first_of(','),
			pos2 = str.find_last_of(',');
			defaultColor.rgba[0] = atof(str.substr(0,pos1).c_str())/255.0;
			defaultColor.rgba[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str())/255.0;
			defaultColor.rgba[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str())/255.0;
		}

		attr= element->Attribute("blocksize");
		if (attr) {
		   string str(attr);
		   int pos1 = str.find_first_of(','),
			   pos2 = str.find_last_of(',');
		   float siz[3];
		   siz[0] = atof(str.substr(0,pos1).c_str());
		   siz[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str());
		   siz[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str());
		   cout << "blocksize =" << siz[0] <<"," << siz[1]<< endl;
		   world->setBlocksSize(siz);
		}

/* Reading a smartblock */
		cout << "default color :" << defaultColor << endl;
		TiXmlNode *block = nodeBlock->FirstChild("block");
		Cell3DPosition position;
		Color color;
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
			   cout << "color :" << defaultColor << endl;
			}
			attr = element->Attribute("position");
			if (attr) {
				string str(attr);
			    int pos = str.find_first_of(',');
			   	position.pt[0] = atoi(str.substr(0,pos).c_str());
			   	position.pt[1] = atoi(str.substr(pos+1,str.length()-pos-1).c_str());
			   	cout << "position : " << position << endl;
			}
			world->addBlock(currentID++,SmartBlocksSimulator::buildNewBlockCode,position,color);
			block = block->NextSibling("block");
		 } // end while (block)

		 block = nodeBlock->FirstChild("blocksLine");
		 int line;
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
			}
			attr = element->Attribute("line");
			if (attr) {
				line = atoi(attr);
			}
			attr = element->Attribute("values");
			if (attr) {
				string str(attr);
				position.pt[1] = line;
			    for(int i=0; i<str.length(); i++) {
			    	if  (str[i]=='1') {
			    		position.pt[0]=i;
			    		world->addBlock(currentID++,SmartBlocksSimulator::buildNewBlockCode,position,color);
			    	}
			    }
			}
			block = block->NextSibling("blocksLine");
		} // end while (nodeBlock)
	} else // end if(nodeBlock)
	{ cerr << "no Block List" << endl;
	}

	TiXmlNode *nodeGrid = node->FirstChild("targetGrid");
 	if (nodeGrid) {
 		TiXmlNode *block = nodeGrid->FirstChild("targetLine");
 		int line;
 		while (block) {
 			TiXmlElement* element = block->ToElement();
 			const char *attr = element->Attribute("line");
 			if (attr) {
 				line = atoi(attr);
 			}
 			attr = element->Attribute("values");
 			if (attr) {
 				string str(attr);
 				for(int i=0; i<str.length(); i++) {
 			    	world->setTargetGrid((str[i]=='1')?fullCell:emptyCell,i,line);
 			    }
 			}
 			block = block->NextSibling("targetLine");
 		}
 	} else {
 		cout << "No target grid" << endl;
 	}

	TiXmlNode *nodeCapa = node->FirstChild("capabilities");
	if (nodeCapa) {
        world->setCapabilities(new SmartBlocksCapabilities(nodeCapa));
    }

 	world->linkBlocks();

	GlutContext::mainLoop();
}

SmartBlocksSimulator::~SmartBlocksSimulator() {
	//MODIF NICO : deleteScheduler() est déjà appellée par BaseSimulator::~Simulator()
	//~ deleteScheduler();
	cout << "\033[1;34m" << "SmartBlocksSimulator destructor" << "\033[0m" <<endl;
}

void SmartBlocksSimulator::createSimulator(int argc, char *argv[],
					   SmartBlocksBlockCode *(*smartBlocksBlockCodeBuildingFunction)
					   (SmartBlocksBlock*)) {
	simulator =  new SmartBlocksSimulator(argc, argv, smartBlocksBlockCodeBuildingFunction);
}

void SmartBlocksSimulator::deleteSimulator() {
	delete ((SmartBlocksSimulator*)simulator);
	simulator = NULL;
}

void SmartBlocksSimulator::loadWorld(int lx, int ly, int lz, int argc, char *argv[]) {
  SmartBlocksWorld::createWorld(lx,ly,argc,argv);
  world = SmartBlocksWorld::getWorld();
  world->loadTextures("../../simulatorCore/smartBlocksTextures");
}

void SmartBlocksSimulator::loadScheduler() {
  SmartBlocksScheduler::createScheduler();
  scheduler = SmartBlocksScheduler::getScheduler();
}


} // SmartBlocks namespace
