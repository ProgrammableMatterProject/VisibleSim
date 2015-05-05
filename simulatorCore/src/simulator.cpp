/*
 * simulator.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#include "simulator.h"
#include "trace.h"
#include "meldProcessVM.h"
#include "meldProcessDebugger.h"
#include "meldInterpretVM.h"
//#include "meldInterpretDebugger.h"

using namespace std;

namespace BaseSimulator {

Simulator* simulator = NULL;

Simulator* Simulator::simulator = NULL;

Simulator::Type	Simulator::type = CPP; // CPP code by default

Simulator::Simulator(int argc, char *argv[]): cmdLine(argc,argv) {

	if (simulator == NULL) {
		simulator = this;
		BaseSimulator::simulator = simulator;
	} else {
		ERRPUT << "\033[1;31m" << "Only one Simulator instance can be created, aborting !" << "\033[0m" << endl;
		exit(EXIT_FAILURE);
	}

	xmlWorldNode = NULL;

	OUTPUT << "\033[1;34m" << "Simulator constructor" << "\033[0m" << endl;
	string confFileName = cmdLine.getConfigFile();

	xmlDoc = new TiXmlDocument(confFileName.c_str());
	bool isLoaded = xmlDoc->LoadFile();

	if (!isLoaded) {
		cerr << "Could not load configuration file :" << confFileName << endl;
		exit(EXIT_FAILURE);
	} else {
		xmlWorldNode = xmlDoc->FirstChild("world");
		if (xmlWorldNode) {
			OUTPUT << "\033[1;34m  " << confFileName << " successfully loaded "<< "\033[0m" << endl;
		} else {
			ERRPUT << "\033[1;31m" << "Could not find root 'world' element in configuration file" << "\033[0m" << endl;
			exit(1);
		}
	}

	if (getType() == MELDPROCESS) {
		TiXmlNode *node = xmlDoc->FirstChild("vm");
		string vmPath;
		string programPath = cmdLine.getProgramPath();
		int port = 0;
		bool debugging = cmdLine.getMeldDebugger();

		if (node) {
			TiXmlElement* vmElement = node->ToElement();
			const char *attr = vmElement->Attribute("serverport");
			if (attr) {
				port = atoi(attr);
			} else {
				cerr << "Error: no port define for the Meld VM" << endl;
				exit(1);
			}
			attr = vmElement->Attribute("vmPath");
			if (attr) {
				vmPath = string(attr);
			} else {
				cerr << "Error: no port define for the Meld VM" << endl;
				exit(1);
			}
			attr = vmElement->Attribute("programPath");
			if (attr) {
			 if (programPath == "") {
				programPath = string(attr);
			 } else {
				cerr << "Warning: meld program provided in the command line and in the xml file" << endl;
				cerr << "Warning: meld program provided in the xml file is ignored" << endl;
			 }
			}
			attr = vmElement->Attribute("debugging");
			if (attr) {
				if ((strcmp(attr, "True") == 0) ||(strcmp(attr, "true") == 0) ) {
					  debugging = true;
				}
			}
		}


		MeldProcess::setVMConfiguration(vmPath, programPath, debugging);
		MeldProcess::createVMServer(port);
		if(debugging) {
			MeldProcess::createDebugger();
		}
	}
	else if(getType() == MELDINTERPRET){
            TiXmlNode *node = xmlDoc->FirstChild("vm");
		string programPath = cmdLine.getProgramPath();
		bool debugging = cmdLine.getMeldDebugger();

		if (node) {
			TiXmlElement* vmElement = node->ToElement();
			const char *attr = vmElement->Attribute("programPath");
			if (attr) {
			 if (programPath == "") {
				programPath = string(attr);
			 } else {
				cerr << "Warning: meld program provided in the command line and in the xml file" << endl;
				cerr << "Warning: meld program provided in the xml file is ignored" << endl;
			 }
			}
			attr = vmElement->Attribute("debugging");
			if (attr) {
				if ((strcmp(attr, "True") == 0) ||(strcmp(attr, "true") == 0) ) {
					  debugging = true;
				}
			}
		}
		OUTPUT << "Loading" << programPath << " with MeldInterpretVM" << endl;
            MeldInterpret::MeldInterpretVM::setConfiguration(programPath, debugging);
            if(debugging){
                  //Don't know what to do yet
            }
	}

}

Simulator::~Simulator() {
	OUTPUT << "\033[1;34m"  << "Simulator destructor" << "\033[0m" << endl;
	//MODIF NICO : le mieux serait de faire ce delete juste après avoir fini de lire le fichier xml
	delete xmlDoc;
	//FIN MODIF NICO

	if (getType() == MELDPROCESS) {
		if(MeldProcess::MeldProcessVM::isInDebuggingMode) {
			MeldProcess::deleteDebugger();
		}
	MeldProcess::deleteVMServer();
	}
	else if (getType() == MELDINTERPRET){
            //Not sure if there is something to do, i think not
	}

	deleteScheduler();
	deleteWorld();
}

} // Simulator namespace
