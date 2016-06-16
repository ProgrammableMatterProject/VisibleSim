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
#include "blinkyBlocksScheduler.cpp"

using namespace std;

namespace BlinkyBlocks {

BlinkyBlocksBlockCode*(* BlinkyBlocksSimulator::buildNewBlockCode)(BlinkyBlocksBlock*)=NULL;

BlinkyBlocksSimulator::BlinkyBlocksSimulator(int argc, char *argv[], BlinkyBlocksBlockCode *(*blinkyBlocksBlockCodeBuildingFunction)(BlinkyBlocksBlock*)) : BaseSimulator::Simulator(argc, argv) {
	OUTPUT << "\033[1;34m" << "BlinkyBlocksSimulator constructor" << "\033[0m" << endl;

	int currentID = 1;
	buildNewBlockCode = blinkyBlocksBlockCodeBuildingFunction;

	parseWorld(argc, argv);
	
	// loading the blocks
	TiXmlNode *nodeBlock = xmlWorldNode->FirstChild("blockList");
	if (xmlWorldNode) {
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
			((BlinkyBlocksWorld*)world)->setBlocksSize(siz);
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
			((BlinkyBlocksWorld*)world)->addBlock(currentID++, BlinkyBlocksSimulator::buildNewBlockCode, position, color);
			nodeBlock = nodeBlock->NextSibling("block");
		} // end while (nodeBlock)
	} else { // end if(nodeBlock)
		ERRPUT << "no Block List" << endl;
	}

	// loading the scenario
	TiXmlNode *nodeScenario = xmlWorldNode->FirstChild("scenario");
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
						((BlinkyBlocksWorld*)world)->addScenarioEvent(new ScenarioTapEvent(eventTime,eventBlockId));
					}
				} else if (strAttr=="debug") {
					attr = element->Attribute("id");
					bool open=true;
					if (attr) {
						string str(attr);
						open = (str=="true" || str=="True");
					}
					OUTPUT << "SCENARIO: debug(" << eventTime << "," << open << ")" << endl;
					((BlinkyBlocksWorld*)world)->addScenarioEvent(new ScenarioDebugEvent(eventTime,open));
				} else if (strAttr=="selectBlock") {
					attr = element->Attribute("id");
					eventBlockId=-1;
					if (attr) {
						eventBlockId=atoi(attr);
					}
					OUTPUT << "SCENARIO: selectBlock(" << eventTime << "," << eventBlockId << ")" << endl;
					((BlinkyBlocksWorld*)world)->addScenarioEvent(new ScenarioSelectBlockEvent(eventTime,eventBlockId));
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
						((BlinkyBlocksWorld*)world)->addScenarioEvent(new ScenarioAddBlockEvent(eventTime,position));
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
	((BlinkyBlocksWorld*)world)->linkBlocks();

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

void BlinkyBlocksSimulator::loadWorld(int lx, int ly, int lz, int argc, char *argv[]) {
	BlinkyBlocksWorld::createWorld(lx,ly,lz,argc,argv);
	world = BlinkyBlocksWorld::getWorld();
	world->loadTextures("../../simulatorCore/blinkyBlocksTextures");
}

void BlinkyBlocksSimulator::loadScheduler() {
	createScheduler();
	scheduler = getScheduler();
}


} // BlinkyBlocks namespace
