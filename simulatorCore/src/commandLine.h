/*
 * commandLine.h
 *
 *  Created on: 26 march 2015
 *      Author: andre
 */

#ifndef COMMANDLINE_H_
#define COMMANDLINE_H_

#include <string>

#include "scheduler.h"

using namespace std;

#define CMD_LINE_UNDEFINED -1

/**
 * CommandLine
 */ 
class CommandLine {
private:
	int topology = CMD_LINE_UNDEFINED;
	int topologyParameter = CMD_LINE_UNDEFINED;
	// int gridSize = 0;

	int schedulerMode = CMD_LINE_UNDEFINED;
	int schedulerLength  = SCHEDULER_LENGTH_DEFAULT;
	bool schedulerAutoStop = false;
	Time maximumDate = 0;
	
	
	bool meldDebugger = false;
	string programPath = "program.bb";
	string vmPath = "";
	int vmPort = 0;
	
	bool stats = false;
	bool fullScreen = false;
	bool terminalOnly = false;
	string configFile = "config.xml";

	bool simulationSeedSet = false;
	int simulationSeed = 0;
	
	void help();
	void read(int argc, char *argv[]);
public:
	CommandLine(int argc, char *argv[]);
	~CommandLine() {};
	
	void print();
	
	bool randomWorldRequested();
	int getRandomTopology() { return topology; }
	int getRandomTopologyParameter() { return topologyParameter; }
	bool getTerminalOnly() { return terminalOnly; }
	int getSchedulerMode() { return schedulerMode; }
	bool getMeldDebugger() { return meldDebugger; }
	string getProgramPath() { return programPath; }
	string getVMPath() { return vmPath; }
	int getVMPort() { return vmPort; }
	string getConfigFile() { return configFile; }
	bool getStats() { return stats; }
	bool getFullScreen() { return fullScreen; }
	// int getGridSize() { return gridSize; }
	int getSchedulerLength() { return schedulerLength; }
	Time getMaximumDate() { return maximumDate; }
	bool getSchedulerAutoStop() { return schedulerAutoStop; }
	bool isSimulationSeedSet() { return simulationSeedSet; }
	int getSimulationSeed() { return simulationSeed; }
};

#endif // CONFIGSTAT_H_
