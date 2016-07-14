/*
 * simulator.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#include "simulator.h"

#include "trace.h"
#include "meldInterpretVM.h"
#include "meldInterpretScheduler.h"
#include "cppScheduler.h"
#include "openglViewer.h"
#include "utils.h"

#ifdef ENABLE_MELDPROCESS
#include "meldProcessVM.h"
#include "meldProcessDebugger.h"
#include "meldProcessScheduler.h"
#endif

using namespace std;

namespace BaseSimulator {

Simulator* simulator = NULL;

Simulator* Simulator::simulator = NULL;

Simulator::Type	Simulator::type = CPP; // CPP code by default

Simulator::Simulator(int argc, char *argv[], BlockCodeBuilder _bcb): cmdLine(argc,argv) {
	OUTPUT << "\033[1;34m" << "Simulator constructor" << "\033[0m" << endl;

	bcb = _bcb;
	
	// Ensure that only one instance of simulator is running at once
	if (simulator == NULL) {
		simulator = this;
		BaseSimulator::simulator = simulator;
	} else {
		ERRPUT << "\033[1;31m" << "Only one Simulator instance can be created, aborting !"
			   << "\033[0m" << endl;
		exit(EXIT_FAILURE);
	}

	// Ensure that the configuration file exists and is well-formed

	string confFileName = cmdLine.getConfigFile();

	xmlDoc = new TiXmlDocument(confFileName.c_str());
	bool isLoaded = xmlDoc->LoadFile();

	if (!isLoaded) {
		cerr << "error: Could not load configuration file :" << confFileName << endl;
		exit(EXIT_FAILURE);
	} else {
		xmlWorldNode = xmlDoc->FirstChild("world");
		if (xmlWorldNode) {
			OUTPUT << "\033[1;34m  " << confFileName << " successfully loaded "
				   << "\033[0m" << endl;
		} else {
			ERRPUT << "\033[1;31m" << "error: Could not find root 'world' element in configuration file"
				   << "\033[0m" << endl;
			exit(EXIT_FAILURE);
		}
	}
}

Simulator::~Simulator() {
	OUTPUT << "\033[1;34m"  << "Simulator destructor" << "\033[0m" << endl;
	//MODIF NICO : le mieux serait de faire ce delete juste après avoir fini de lire le fichier xml
	delete xmlDoc;
	//FIN MODIF NICO

#ifdef ENABLE_MELDPROCESS
	if (getType() == MELDPROCESS) {
		if(MeldProcess::MeldProcessVM::isInDebuggingMode()) {
			MeldProcess::deleteDebugger();
		}
		MeldProcess::deleteVMServer();
	}
#endif
	
	if (getType() == MELDINTERPRET){
		//Not sure if there is something to do, i think not
	}

	// deleteScheduler();
	deleteWorld();
}

void Simulator::deleteSimulator() {
    delete simulator;
    simulator = NULL;
}

void Simulator::loadScheduler(int schedulerMaxDate) {
	int sl = cmdLine.getSchedulerLength();
	int sm = cmdLine.getSchedulerMode();
	
	// Create a scheduler of the same type as target BlockCode
	switch(getType()) {
	case MELDINTERPRET:
		MeldInterpret::MeldInterpretScheduler::createScheduler();
		break;
	case MELDPROCESS:
#ifdef MELDPROCESSVM
		MeldProcess::MeldProcessScheduler::createScheduler();
#else
		cerr << "error: MeldProcess not compiled in this version. "
			 << "Please enable MELDPROCESS in simulatorCore/src/Makefile to enable it, and try again"
			 << endl;
		exit(EXIT_FAILURE);
#endif
		break;
	case CPP:
		CPPScheduler::createScheduler();
		break;
	}

	scheduler = getScheduler();
	
	// Set the scheduler execution mode on start, if enabled	
	if (sm != CMD_LINE_UNDEFINED) {
		scheduler->setSchedulerMode(sm);
		scheduler->setAutoStart(true);
	}
	
	// Set the scheduler termination mode
	scheduler->setSchedulerLength(sl);
	scheduler->setAutoStop(cmdLine.getSchedulerAutoStop());

	if (sl == SCHEDULER_LENGTH_BOUNDED) {
		scheduler->setMaximumDate(cmdLine.getMaximumDate());
	}
}

void Simulator::parseConfiguration(int argc, char*argv[]) {
	// Identify the type of the simulation (CPP / Meld Process / MeldInterpret)
	readSimulationType(argc, argv);	

	// Configure the simulation world
	parseWorld(argc, argv);	
	
	// Instantiate and configure the Scheduler
	loadScheduler(schedulerMaxDate);

	// Parse and configure the remaining items
	parseCameraAndSpotlight();
	parseBlockList();
	parseTarget();
}

void Simulator::readSimulationType(int argc, char*argv[]) {
#ifdef ENABLE_MELDPROCESS
	if (getType() == MELDPROCESS) {
		string vmPath = cmdLine.getVMPath();
		string programPath = cmdLine.getProgramPath();
		int vmPort = cmdLine.getVMPort();
		bool debugging = cmdLine.getMeldDebugger();

		if (vmPath == "") {
			cerr << "error: no path defined for Meld VM" << endl;
			exit(1);
		} else if (!vmPort) {
			cerr << "error: no port defined for Meld VM" << endl;
			exit(1);
		} else if (!file_exists(programPath)) {
			cerr << "error: no Meld program was provided, or default file \"./program.bb\" does not exist"
				 << endl;
			exit(1);
		}
		
		MeldProcess::setVMConfiguration(vmPath, programPath, debugging);
		MeldProcess::createVMServer(vmPort);
		if(debugging) {
			MeldProcess::createDebugger();
		}

		return;
	}
#endif
	
	if(getType() == MELDINTERPRET) {
		string programPath = cmdLine.getProgramPath();
		bool debugging = cmdLine.getMeldDebugger();

		if (!file_exists(programPath)) {
			cerr << "error: no Meld program was provided, or default file \"./program.bb\" does not exist"
				 << endl;
			exit(1);
		}
		OUTPUT << "Loading " << programPath << " with MeldInterpretVM" << endl;
		MeldInterpret::MeldInterpretVM::setConfiguration(programPath, debugging);
		if(debugging){
			//Don't know what to do yet
			cerr << "warning: MeldInterpreter debugging not implemented yet" << endl;
		}
	}
}

void Simulator::parseWorld(int argc, char*argv[]) {		
	/* reading the xml file */
	xmlWorldNode = xmlDoc->FirstChild("world");

	if (xmlWorldNode) {
		TiXmlElement* worldElement = xmlWorldNode->ToElement();
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

		attr = worldElement->Attribute("windowSize");
		if (attr) {
			string str=attr;
			int pos = str.find_first_of(',');
			GlutContext::initialScreenWidth = atoi(str.substr(0,pos).c_str());
			GlutContext::initialScreenHeight = atoi(str.substr(pos+1,str.length()-pos-1).c_str());
			GlutContext::screenWidth = GlutContext::initialScreenWidth;
			GlutContext::screenHeight = GlutContext::initialScreenHeight;
		}

		attr=worldElement->Attribute("maxSimulationTime");
		if (attr) {
			string str=attr;
			uint64_t t = atoi(attr);
			int l = strlen(attr);
			if (str.substr(l-2,2)=="mn") {
				t*=60000000;
			} else if (str.substr(l-2,2)=="ms") {
				t*=1000;
			} else if (str.substr(l-1,1)=="s") {
				t*=1000000;
			}

			schedulerMaxDate = t;
			cerr << "warning: maxSimulationTime in the configuration is not supported anymore,"
				 << " please use the command line option [-s <maxTime>]" << endl;
		}

		// Get Blocksize
		float blockSize[3] = {0.0,0.0,0.0};
		xmlBlockListNode = xmlWorldNode->FirstChild("blockList");
		if (xmlBlockListNode) {
			TiXmlElement *blockListElement = xmlBlockListNode->ToElement();
			attr = blockListElement->Attribute("blockSize");
			if (attr) {
				string str(attr);
				int pos1 = str.find_first_of(','),
					pos2 = str.find_last_of(',');
				blockSize[0] = atof(str.substr(0,pos1).c_str());
				blockSize[1] = atof(str.substr(pos1+1,pos2-pos1-1).c_str());
				blockSize[2] = atof(str.substr(pos2+1,str.length()-pos1-1).c_str());
				OUTPUT << "blocksize =" << blockSize[0] << "," << blockSize[1] << "," << blockSize[2] << endl;
			}
		}

		// Create the simulation world and lattice
		loadWorld(Cell3DPosition(lx,ly,lz),
				  Vector3D(blockSize[0], blockSize[1], blockSize[2]), argc, argv);
	} else {
		ERRPUT << "ERROR : No world in XML configuration file" << endl;
		exit(1);
	}
}

void Simulator::parseCameraAndSpotlight() {
	if (GlutContext::GUIisEnabled) {		
		// loading the camera parameters
		TiXmlNode *nodeConfig = xmlWorldNode->FirstChild("camera");
		if (nodeConfig) {
			TiXmlElement* cameraElement = nodeConfig->ToElement();
			const char *attr=cameraElement->Attribute("target");
			double def_near=1,def_far=1500;
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
				// def_near = dist-az;
				// def_far = dist+az;
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
		nodeConfig = xmlWorldNode->FirstChild("spotlight");
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
	}
}

void Simulator::parseBlockList() {
	int currentID = 1;

	TiXmlElement* element = xmlBlockListNode->ToElement();
	if (xmlBlockListNode) {
		Color defaultColor = DARKGREY;
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
#if 1
		/* Reading a catoms */
		TiXmlNode *block = xmlBlockListNode->FirstChild("block");
		Cell3DPosition position;
		Color color;
		bool master;
		while (block) {
			element = block->ToElement();
			color=defaultColor;
			master=false;
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
			attr = element->Attribute("position");
			if (attr) {
				string str(attr);
				int pos = str.find_first_of(',');
				int pos2 = str.find_last_of(',');
				int ix = atof(str.substr(0,pos).c_str()),
					iy = atoi(str.substr(pos+1,pos2-pos-1).c_str()),
					iz = atoi(str.substr(pos2+1,str.length()-pos2-1).c_str());

				// cerr << ix << "," << iy << "," << iz << endl;

				position.pt[0] = ix;
				position.pt[1] = iy;
				position.pt[2] = iz;
			}
			attr = element->Attribute("master");
			if (attr) {
				string str(attr);
				if (str.compare("true")==0 || str.compare("1")==0) {
					master=true;
				}
				OUTPUT << "master : " << master << endl;
			}

			// cerr << "addBlock(" << currentID << ") pos = " << position << endl;
			loadBlock(element, currentID++, bcb, position, color, master);

			block = block->NextSibling("block");
		} // end while (block)

		block = xmlBlockListNode->FirstChild("blocksLine");
		int line = 0, plane = 0;
		while (block) {
			line = 0;
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
				position.pt[0] = 0;
				position.pt[1] = line;
				position.pt[2] = plane;
				int n = str.length();
				for(int i=0; i<n; i++) {
					if  (str[i]=='1') {
						position.pt[0]=i;
						loadBlock(element, currentID++, bcb, position, color, false);
					}
				}
			}
			block = block->NextSibling("blocksLine");
		} // end while (nodeBlock)*/
#endif
	} else { // end if(nodeBlock)
		cerr << "warning: no Block List in configuration file" << endl;
	}
}

void Simulator::parseTarget() {
	TiXmlNode *nodeGrid = xmlWorldNode->FirstChild("targetGrid");
	vector<Cell3DPosition> targetCells; // Locations of all target full cells

	if (nodeGrid) {
		TiXmlNode *block = nodeGrid->FirstChild("block");
		Cell3DPosition position;
		const char *attr;
		TiXmlElement* element;
		while (block) {
			element = block->ToElement();
			attr = element->Attribute("position");
			if (attr) {
				string str(attr);
				int pos = str.find_first_of(',');
				int pos2 = str.find_last_of(',');
				int ix = atof(str.substr(0,pos).c_str()),
					iy = atoi(str.substr(pos+1,pos2-pos-1).c_str()),
					iz = atoi(str.substr(pos2+1,str.length()-pos2-1).c_str());

				position.pt[0] = ix;
				position.pt[1] = iy;
				position.pt[2] = iz;

				targetCells.push_back(Cell3DPosition(position[0], position[1], position[2]));
			}

			block = block->NextSibling("block");
		}

		block = nodeGrid->FirstChild("targetLine");
		int line = 0, plane = 0;
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
				for(int i = 0; i < n; i++) {
					if(str[i] == '1') {
						targetCells.push_back(Cell3DPosition(i, line, plane));
					}
				}
			}

			block = block->NextSibling("targetLine");
		}
	} else {
		ERRPUT << "warning: No target grid in configuration file" << endl;
	}

	loadTargetAndCapabilities(targetCells);
}

void Simulator::startSimulation(void) {
	// Connect all blocks – TODO: Check if needed to do it here (maybe all blocks are linked on addition)
	world->linkBlocks();
	
	// Finalize scheduler configuration and start simulation if autoStart is enabled
	Scheduler *scheduler = getScheduler();
	//scheduler->sem_schedulerStart->post();
	scheduler->setState(Scheduler::NOTSTARTED);
	if (scheduler->willAutoStart())
		scheduler->start(scheduler->getSchedulerMode()); 

	// Enter graphical main loop
	GlutContext::mainLoop();
}

} // Simulator namespace
