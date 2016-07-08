/*
 * commandLine.h
 *
 *  Created on: 26 march 2015
 *      Author: andre
 */

#ifndef COMMANDLINE_H_
#define COMMANDLINE_H_

#include <string>

using namespace std;

#define CMD_LINE_UNDEFINED -1

/**
 * CommandLine
 */ 
class CommandLine {
private:
	int topology;
	int topologyParameter;
	bool terminalOnly;
	int schedulerMode;
	int schedulerLength;
	uint64_t maximumDate;
	bool meldDebugger;
	string programPath;
	string vmPath;
	int vmPort;
	string configFile;
	bool stats;
	bool fullScreen;
	int gridSize;
	
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
	int getGridSize() { return gridSize; }
	int getSchedulerLength() { return schedulerLength; }
	uint64_t getMaximumDate() { return maximumDate; }
};

#endif // CONFIGSTAT_H_
