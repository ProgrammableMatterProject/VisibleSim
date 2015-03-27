/*
 * clock.h
 *
 *  Created on: 26 march 2015
 *      Author: andre
 */

#ifndef COMMANDLINE_H_
#define COMMANDLINE_H_

#include <string>

using namespace std;

/**
 * CommandLine
 */
 
class CommandLine {
private:
	int topology;
	int topologyParameter;
	bool terminalOnly;
	int schedulerMode;
	bool meldDebugger;
	string programPath;
	
	void help();
	void read(int argc, char *argv[]);

public:
	CommandLine(int argc, char *argv[]);
	~CommandLine() {};
	
	void print();
};

#endif // CONFIGSTAT_H_
