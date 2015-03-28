/*
 * simulator.cpp
 *
 *  Created on: 22 mars 2013
 *      Author: dom
 */

#include "simulator.h"
#include "trace.h"

using namespace std;

namespace BaseSimulator {

Simulator* simulator = NULL;

Simulator* Simulator::simulator = NULL;

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
	
   /*for (int i=1; i < argc; i++) {
      if(!strcmp(argv[i], "-c")) {
         if (i+1 < argc) {
            confFileName= argv[i+1];
            break;
         } else {
            cerr << "Provide a configuration file after -c" << endl;
            exit(EXIT_FAILURE);
         }
      }
   }*/
   
	

	xmlDoc = new TiXmlDocument(confFileName.c_str());

	bool isLoaded = xmlDoc->LoadFile();

	if ( !isLoaded) {
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
}

Simulator::~Simulator() {
	OUTPUT << "\033[1;34m"  << "Simulator destructor" << "\033[0m" << endl;
	//MODIF NICO : le mieux serait de faire ce delete juste après avoir fini de lire le fichier xml
	delete xmlDoc;
	//FIN MODIF NICO
	deleteScheduler();
	deleteWorld();
}

} // Simulator namespace
