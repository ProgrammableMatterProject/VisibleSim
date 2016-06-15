/*
* blinkyBlocksSimulator.cpp
*
*  Created on: 23 mars 2013
*      Author: dom
*/

#include <iostream>
#include "blinkyBlocksSimulator.h"
#include <string.h>
#include "trace.h"

using namespace std;

namespace BlinkyBlocks {

BlinkyBlocksBlockCode*(* BlinkyBlocksSimulator::buildNewBlockCode)(BlinkyBlocksBlock*)=NULL;

BlinkyBlocksSimulator::BlinkyBlocksSimulator(int argc, char *argv[], BlinkyBlocksBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)(BlinkyBlocksBlock*)) : BaseSimulator::Simulator(argc, argv) {
	OUTPUT << "\033[1;34m" << "BlinkyBlocksSimulator constructor" << "\033[0m" << endl;

	int currentID = 1;
	BlinkyBlocksWorld *world = NULL;
	buildNewBlockCode = blinkyBlocksBlockCodeBuildingFunction;

    // PThy: Reading the command line may be needed at this time to enable debugging.
    
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
		world->loadTextures("../../simulatorCore/blinkyBlocksTextures");

	} else {
		ERRPUT << "ERROR : NO world in XML file" << endl;
		exit(1);
	}

	createScheduler();

    // Pthy: Not sure if we should be keeping that debugging part, perhaps it has to be located somewhere else now that MeldInterpreter has been embedded into VSim
//    if(debugging) {
//		createDebugger();
//	}

	// loading the camera parameters
	TiXmlNode *nodeConfig = node->FirstChild("camera");
	if (nodeConfig) {
		TiXmlElement* cameraElement = nodeConfig->ToElement();
		const char *attr=cameraElement->Attribute("target");
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
		}
		attr=cameraElement->Attribute("angle");
		if (attr) {
			float angle = atof(attr);
			world->getCamera()->setAngle(angle);
		}
		double def_near=1,def_far=1500;
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
		float farplane=2.0*dist*tan(angle*M_PI/180.0);
		world->getCamera()->setLightParameters(target,az,ele,dist,angle,10.0,farplane);

	}

	// loading the blocks
	TiXmlNode *nodeBlock = node->FirstChild("blockList");
	if (nodeBlock) {
		Color defaultColor = DARKGREY;
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
		attr= element->Attribute("blockSize");
		if (attr) {
			string str(attr);
			int pos1 = str.find_first_of(','),
			pos2 = str.find_last_of(',');
			float siz[3];
			siz[0] = atof(str.substr(0,pos1).c_str());
			siz[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str());
			siz[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str());
			OUTPUT << "blockSize =" << siz[0] <<"," << siz[1] <<"," << siz[2]<< endl;
			world->setBlocksSize(siz);
		}

		/* Reading a blinkyblock */
		OUTPUT << "default color :" << defaultColor << endl;
		nodeBlock = nodeBlock->FirstChild("block");
		Cell3DPosition position;
		Color color;
		while (nodeBlock) {
			element = nodeBlock->ToElement();
			color=defaultColor;
			attr = element->Attribute("color");
			if (attr) {
				string str(attr);
				int pos1 = str.find_first_of(','),
				pos2 = str.find_last_of(',');
				 color.set(atof(str.substr(0,pos1).c_str())/255.0,
                         atof(str.substr(pos1+1,pos2-pos1-1).c_str())/255.0,
                         atof(str.substr(pos2+1,str.length()-pos1-1).c_str())/255.0);
				OUTPUT << "color :" << color << endl;
			}
			attr = element->Attribute("position");
			if (attr) {
				string str(attr);
				int pos1 = str.find_first_of(','),
				pos2 = str.find_last_of(',');
				position.pt[0] = atoi(str.substr(0,pos1).c_str());
				position.pt[1] = atoi(str.substr(pos1+1,pos2-pos1-1).c_str());
				position.pt[2] = atoi(str.substr(pos2+1,str.length()-pos1-1).c_str());
				OUTPUT << "position : " << position << endl;
			}
			world->addBlock(currentID++, BlinkyBlocksSimulator::buildNewBlockCode, position, color);
			nodeBlock = nodeBlock->NextSibling("block");
		} // end while (nodeBlock)
	} else { // end if(nodeBlock)
		ERRPUT << "no Block List" << endl;
	}

	// loading the scenario
	TiXmlNode *nodeScenario = node->FirstChild("scenario");
	if (nodeScenario) {
		bool autostart=false;
		TiXmlElement* element = nodeScenario->ToElement();
		const char *attr= element->Attribute("autostart");
		if (attr) {
			string str(attr);
			autostart=(str=="True" || str=="true");
		}
		OUTPUT << "SCENARIO: Autostart=" << autostart << endl;
		/* Reading an event */
		nodeScenario = nodeScenario->FirstChild("event");
		float eventTime =0.0;
		int eventBlockId=-1;
		while (nodeScenario) {
			element = nodeScenario->ToElement();
			attr = element->Attribute("time");
			if (attr) {
				eventTime = atof(attr);
			}
			attr = element->Attribute("type");
			if (attr) {
				string strAttr(attr);
				if (strAttr=="tap") {
					attr = element->Attribute("id");
					eventBlockId=-1;
					if (attr) {
						eventBlockId=atoi(attr);
					}
					if (eventBlockId==-1) {
						ERRPUT << "SCENARIO:No id for tap event" << endl;
					} else {
						OUTPUT << "SCENARIO: tap(" << eventTime << "," << eventBlockId << ")" << endl;
						world->addScenarioEvent(new ScenarioTapEvent(eventTime,eventBlockId));
					}
				} else if (strAttr=="debug") {
					attr = element->Attribute("id");
					bool open=true;
					if (attr) {
						string str(attr);
						open = (str=="true" || str=="True");
					}
					OUTPUT << "SCENARIO: debug(" << eventTime << "," << open << ")" << endl;
					world->addScenarioEvent(new ScenarioDebugEvent(eventTime,open));
				} else if (strAttr=="selectBlock") {
					attr = element->Attribute("id");
					eventBlockId=-1;
					if (attr) {
						eventBlockId=atoi(attr);
					}
					OUTPUT << "SCENARIO: selectBlock(" << eventTime << "," << eventBlockId << ")" << endl;
					world->addScenarioEvent(new ScenarioSelectBlockEvent(eventTime,eventBlockId));
				} else if (strAttr=="addBlock") {
					attr = element->Attribute("position");
					if (attr) {
						string str(attr);
						int pos1 = str.find_first_of(','),
						pos2 = str.find_last_of(',');
						Vector3D position;
						position.pt[0] = atoi(str.substr(0,pos1).c_str());
						position.pt[1] = atoi(str.substr(pos1+1,pos2-pos1-1).c_str());
						position.pt[2] = atoi(str.substr(pos2+1,str.length()-pos1-1).c_str());
						OUTPUT << "SCENARIO: addBlock(" << eventTime << "," << position << ")" << endl;
						world->addScenarioEvent(new ScenarioAddBlockEvent(eventTime,position));
					} else {
						ERRPUT << "SCENARIO: No position for addBlock event" << endl;
					}

				} else {
					ERRPUT << "SCENARIO: event '" << attr << "': unknown !" << endl;
				}

			} else {
				ERRPUT << "SCENARIO: no Event type " << endl;
			}
			nodeScenario = nodeScenario->NextSibling("event");
		} // while(nodeScenario)
	}
	world->linkBlocks();

	//getScheduler()->sem_schedulerStart->post();
	getScheduler()->setState(Scheduler::NOTSTARTED);

	GlutContext::mainLoop();

}

BlinkyBlocksSimulator::~BlinkyBlocksSimulator() {
	OUTPUT << "\033[1;34m" << "BlinkyBlocksSimulator destructor" << "\033[0m" <<endl;
}

void BlinkyBlocksSimulator::createSimulator(int argc, char *argv[], BlinkyBlocksBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)(BlinkyBlocksBlock*)) {
	simulator =  new BlinkyBlocksSimulator(argc, argv, blinkyBlocksBlockCodeBuildingFunction);
}

void BlinkyBlocksSimulator::deleteSimulator() {
	delete((BlinkyBlocksSimulator*)simulator);
}

} // BlinkyBlocks namespace
