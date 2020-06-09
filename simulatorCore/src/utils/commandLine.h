/*
 * commandLine.h
 *
 *  Created on: 26 march 2015
 *      Author: andre
 */

#ifndef COMMANDLINE_H_
#define COMMANDLINE_H_

#include <string>

#include "events/scheduler.h"
#include "utils/tDefs.h"

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
    string appName;

    bool replayEnabled = false;
    string replayFilename;

    bool simulationSeedSet = false;
    int simulationSeed = 0;

    void help() const;
    void read(int argc, char *argv[], BlockCodeBuilder bcb);
public:
    CommandLine(int argc, char *argv[], BlockCodeBuilder bcb);
    ~CommandLine() {};

    void print() const;

    string getApplicationName() const { return appName; }

    bool isReplayEnabled() const{ return replayEnabled; }
    string getReplayFilename() const { return replayFilename; }

    bool randomWorldRequested() const;
    int getRandomTopology() const { return topology; }
    int getRandomTopologyParameter() const { return topologyParameter; }
    bool getTerminalOnly() const { return terminalOnly; }
    int getSchedulerMode() const { return schedulerMode; }
    bool getMeldDebugger() const { return meldDebugger; }
    string getProgramPath() const { return programPath; }
    string getVMPath() const { return vmPath; }
    int getVMPort() const { return vmPort; }
    string getConfigFile() const { return configFile; }
    bool getStats() const { return stats; }
    bool getFullScreen() const { return fullScreen; }
    // int getGridSize() const { return gridSize; }
    int getSchedulerLength() const { return schedulerLength; }
    Time getMaximumDate() const { return maximumDate; }
    bool getSchedulerAutoStop() const { return schedulerAutoStop; }

    bool isSimulationSeedSet() const { return simulationSeedSet; }
    int getSimulationSeed() const { return simulationSeed; }

    /**
     * @brief Search option -k in the command line arguments to deduce the target module type
     * @return ModuleType enum value (defined in tDefs.h) for target module type specified by -k option if present.
     *         Terminate simulation if missing or unknown value.
     */
    static ModuleType readModuleType(int argc, char **argv);
};

#endif // CONFIGSTAT_H_
